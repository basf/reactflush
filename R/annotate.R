#' annotate all inputs/reactives for each code block
#'
#' @description identify and annotate all inputs/reactives referenced in code block.
#'   Take isolated inputs/reactives into account
#'
#' @param blocks \code{data.frame} all code blocks in server.R code
#' @param inpts \code{vector} of all inputs/reactives
#' @param type \code{string} which type to annotate: inputs or reactives
#'
#' @importFrom rlang .data
#' @import dplyr purrr
annotate_type <- function(blocks, inpts, type = c("inputs","reactives")){
  type <- rlang::arg_match(type)

  blocks <- blocks %>%
    mutate(
      type_expr = map(expr, list_matches, matches = inpts),
      type_opts = map(options, list_matches, matches = inpts),
      type_islt = map(isolate, list_matches, matches = inpts),
      type_deps = pmap(
        list(type, type_expr, type_opts),
        determine_invalidators
      )
    ) %>%
    rename(
      !! sep_(type, "expr") := .data$type_expr,
      !! sep_(type, "opts") := .data$type_opts,
      !! sep_(type, "islt") := .data$type_islt,
      !! sep_(type, "deps") := .data$type_deps
    )

  return(blocks)
}


#' annotate all side effects for each code block
#'
#' @description identify and annotate all side effects triggered in code block.
#'   Take both reactiveVal() and values$reactiveValues into account
#'
#' @param blocks \code{data.frame} all code blocks in server.R code
#' @param reactives \code{vector} of all reactives, both reactiveVal() and values$reactiveValues
#'
#' @importFrom rlang .data
#' @import dplyr purrr
annotate_side_effects <- function(blocks, reactives) {
  # extract the name of the reactiveValues
  reactive_value <- blocks %>%
    filter(.data$type == "reactiveValues") %>%
    pull(.data$name)

  # match code with reactive side effects
  blocks <- blocks %>%
    mutate(
      side_effects_deps = map(body, list_side_effects, matches = reactives, reactive_value = reactive_value)
    )

  return(blocks)
}


#' annotate all input defentions in code blocks WIP
#'
#' @description identify and annotate all defenitions within code block. WIP
annotate_definitions <- function() {
  #(?:Input|Box)\([^)]*['"]timepoint_analysis['"]
}


#' list all occuring matches in code block
#'
#' @description list all occuring matches in code block
#'
#' @param code \code{string} code of code block
#' @param matches \code{vector} of all inputs/reactives
#'
#' @importFrom magrittr "%>%"
#' @import dplyr stringr
list_matches <- function(code, matches) {
  code %>%
    str_extract(pattern = fixed(matches)) %>%
    enframe() %>%
    filter(!is.na(value)) %>%
    pull(value) %>%
    str_replace("\\(\\)","") %>%
    str_trim()
}


#' list all occuring side effects in code block
#'
#' @description list all occuring matches in code block
#'
#' @param code \code{string} code of code block
#' @param matches \code{vector} of all reactives, both reactiveVal() and values$reactiveValues
#' @param reactive_value \code{string} name of reactiveValues() object, defautls to "values"
#' @param regex_assgn \code{string} regex to detect assignments
#' @param regex_braces \code{string} regex to detect assignment in braces
#'
#' @importFrom magrittr "%>%"
#' @import dplyr tibble stringr
list_side_effects <- function(code, matches, reactive_value = "values", regex_assgn = regex_pattern()$assignment, regex_braces = regex_pattern()$braces_start) {
  # if there are no reactives, we will not find side effects
  if(identical(matches, character(0))) {
    return(character())
  }

  no_r_name <- "[^a-zA-Z0-9._]"
  # seperate reactiveValues values$<name> from reactiveVals <name>(..)
  # also remove last two () characters from reactiveVals
  reactive_vals <- matches[!str_detect(matches, paste0(reactive_value, "\\$"))] %>% str_sub(end = -3)
  reactive_values <- matches[str_detect(matches, paste0(reactive_value, "\\$"))] %>% str_replace(fixed("$"), fixed("\\$"))

  # concate <name> with assignemnt pattern: (...
  # add to no_r_name part to avoid selected_values(...) being matched as values(...)
  regex_reactiveVals_assgn <- paste0(no_r_name, "(", reactive_vals, ")", regex_braces)

  matches <- code %>%
    str_extract(pattern = regex_reactiveVals_assgn) %>%
    enframe() %>%
    filter(!is.na(value))

  if(nrow(matches) == 0) {
    matches <- character()
  } else {
    matches <- matches %>%
      mutate(value = str_split(value, "\\(", n = 2, simplify = TRUE)[,1]) %>%
      pull(value)
  }

  # if no reactiveValues are detected, skip this step
  if(!identical(reactive_value, character(0))) {
    # concate values$<name> with assignemnt pattern: values$<name> <- or =
    regex_reactiveValues_assgn <- paste0("(",reactive_values, ")", regex_assgn)

    matches <- code %>%
      str_match(pattern = regex_reactiveValues_assgn) %>%
      .[,2] %>%
      enframe() %>%
      filter(!is.na(value)) %>%
      pull(value) %>%
      c(matches)

  }

  return(matches)
}


#' determine reactive invalidators
#'
#' @description list all reactive invalidators in code block, depending on type and where
#'   within the code the inputs/reactives are used
#'
#' @param type \code{string} type of code block (reactive, observeEvent, ...)
#' @param expr \code{vector} inputs/reactives referenced in main expression
#' @param opts \code{vector} inputs/reactives referenced in options accompanying expression
#'
#' @importFrom magrittr "%>%"
#' @import stringr
determine_invalidators <- function(type, expr, opts) {
  # depending on the type the block is invalidated either expr, opts or both or none
  if(str_detect(type, fixed("event", ignore_case=TRUE))) {
    # observeEvent & eventReactive only are only invalidated by options, not
    # their expression
    return(opts)
  } else if(type == "function"){
    # functions do not get invalidated
    return(character())
  } else {
    # reactive, observe & renders are invalidated by both expr and opts
    return(c(expr, opts) %>% unique())
  }
}
