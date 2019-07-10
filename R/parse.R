#' parse shiny app located within directory
#'
#' @param dir \code{string} directory where at least a server.R file is present
#'
#' @import fs readr dplyr
#' @export
parse_shiny_app <- function(dir) {

  server_path <- fs::path(dir, "server.R")
  if(!fs::file_exists(server_path)) {
    server_path <- fs::path(dir, "app.R")
    assertthat::assert_that(fs::file_exists(server_path))
  }
  server_file <- readr::read_file(server_path)
  server <- parse_shiny_server(server_file, dir)

  #cat(server$code[1])

  server <- server %>%
    mutate(
      blocks = map2(blocks, inputs, annotate_type, type = "inputs"),
      blocks = map2(blocks, reactives, annotate_type, type = "reactives"),
      blocks = map2(blocks, reactives, annotate_side_effects)
    )

  app <- server %>% tidyr::unnest(blocks, .drop = FALSE)

  ui_path <- fs::path(dir, "ui.R")
  if(fs::file_exists(ui_path)) {
    ui_file <- readr::read_file(ui_path)
  }

  return(app)

}


#' parse code of server.R
#'
#' @description parse sever.R code, inline replace source statements, extract
#'   name of input, output & server; list all used inputs and reactives;
#'   construct data.frame of seperate code blocks
#'
#' @param server_file \code{string} content of server.R file
#' @param dir \code{string} directory of server.R file
#'
#' @import dplyr purrr
#' @export
parse_shiny_server <- function(server_file, dir) {

  # fetch all code within the server definition
  # and parse the names for input, output and session
  # function(<input> , <output>, <sesion> ) { <code> }
  server <- parse_shiny_server_code(server_file)

  if(nrow(server) > 1) {
    stop("Multiple (", nrow(server), ") function definitions found in server.R
  Global functions should be in global.R")
  }

  server <- server %>%
    mutate(
      # recursively replace source statements inline with sourced code
      code = parse_source_statements(code, dir),
      # parse code blocks and
      blocks = map(code, parse_code_blocks),
      # parse and extract vector of inputs
      inputs = map2(code, input, extract_inputs),
      # parse and extract vector of reactives
      reactives = map2(code, blocks, extract_reactives)
    )

  return(server)
}


#' extract code within the server definition
#'
#' @description get shiny code with server defintion \code{function(<input> , <output>, <sesion> ) { <code> }}
#'   and remove comments
#'
#' @param code \code{string} server.R code
#' @param regex \code{string} regex pattern to detect \code{function(<input> , <output>, <sesion> ) { <code> }}
#'
#' @importFrom magrittr "%>%"
#' @import stringr
parse_shiny_server_code <- function(code, regex = regex_pattern()$shiny_server) {

  code %>%
    # remove comments
    str_replace_all("\\n *#.*","") %>%
    # extract code from shiny server function
    str_match_all_perl(regex) %>%
    # set colum names
    magrittr::set_colnames(c("server", "input", "output", "session","code"))
}


#' recursively inline replace shiny source statements
#'
#' @description detect all source statements used to split shiy server into separate
#'   files and replace them inline with the actual sourced code. Do this recursively
#'   for sourced files so we make sure there are no source statements left in the
#'   returned code
#' @param code \code{string} server.R code
#' @param dir \code{string} path of server.R file
#' @param regex \code{string} regex pattern to detect source statements
#'
#' @importFrom stats setNames
#' @import stringr dplyr purrr fs
parse_source_statements <- function(code, dir, regex = regex_pattern()$source_pattern) {

  # extract source statements
  sources <- code %>%
    str_match_all_perl(regex)

  # if no source statments are detected we can return the code as is
  if(nrow(sources) == 0) {
    return(code)
  }

  # read file from source statement
  sources <- sources %>%
    # set column names
    magrittr::set_colnames(c("match", "path")) %>%
    mutate(
      dir_parent = dir,
      # create file & dir path
      file = fs::path(dir, path),
      dir  = fs::path_dir(file),
      # read files
      code = map(file, readr::read_file),
      # surround by line breaks (to make sure we catch all comments)
      code = paste("\n", code, "\n"),
      # remove comments
      code = code %>% str_replace_all("\\n *#.*","")
    ) %>%
    select(match, code, dir, dir_parent)

  # TODO extract chdir=T argument and pass dir instead of dir_parent if true
  # recursively extract source in sourced files

  # TODO figure out why purrr does not replace nested source statements
  # sources %>%
  #   mutate(code = map2(code, dir_parent, parse_source_statements))

  for(i in 1:nrow(sources)) {
    new_code <- parse_source_statements(sources$code[i], sources$dir_parent[i])
    sources$code[i] <- new_code
  }
  # replace source statement with code
  code <-
    str_replace_all(
      code,
      setNames(fixed(sources$code), sources$match)
    )

  return(code)
}


#' extract code blocks from server.R code
#'
#' @description identify, annotate and extract code blocks in shiny server.R code.
#'   Annotating the name, function, body, options, expr and isolates
#'   \code{<name> <- <function>\( <body> <options> \{ <expr> <isolates>\} <options>\) }
#'
#' @param code \code{string} server.R code
#' @param regex_function \code{string} regex pattern to detect function blocks
#' @param regex_block \code{string} regex pattern to detect code blocks
#'
#' @import stringr dplyr purrr
parse_code_blocks <- function(code, regex_function = regex_pattern()$function_pattern, regex_block = regex_pattern()$shiny_server_blocks) {

  blocks <- code %>%
    # extract all code blocks
    str_match_all_perl(regex_block) %>%
    # set colum names
    magrittr::set_colnames(c("match", "name", "object", "type", "body", "expr", "func_body")) %>%
    # remove functions since they are not reactive
    filter(type != "function")

  blocks %>%
    mutate(
      # remove <library>:: part from all types
      type = gsub("^.*::","",type),
      # extract options, everything outside expr {}
      options = map2(body, expr, parse_options),
      # extract all isolates within expr and concatenate in one string
      isolate = map2(expr, type, extract_isolates),
      # remove all isolates within expr
      expr = map2(expr, type, delete_isolates)
    ) %>%
    # create block ids
    group_by(type) %>%
    mutate(id = paste("m",type,row_number(), sep = "_")) %>%
    ungroup()
}


#' extract options from code shiny server code blocks
#'
#' @description extract and concat options from code block
#'   \code{\( <options> \{ <expr>\} <options>\) } by simple substraction of the
#'   expression from the code block
#'
#' @param body \code{string} body of code block
#' @param expr \code{string} the expression within the code block
#'
#' @import stringr
parse_options <- function(body, expr) {
  ifelse(expr == "", body, str_replace(body, fixed(expr), ""))
}
