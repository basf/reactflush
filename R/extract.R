
#' extract all inputs from shiny server code
#'
#' @description extract all inputs using regex from shiny server code
#'
#' @param code \code{string} server.R code
#' @param input \code{string} name of reactie input list, defaults to "input"
#' @param regex \code{string} regex pattern to detect reactive inputs
#'
#' @import dplyr
extract_inputs <- function(code, input = "input", regex = regex_pattern()$input_pattern) {
  inputs <- code %>%
    str_match_all_perl(glue::glue(input,regex)) %>%
    distinct()

  if(nrow(inputs) == 0) {
    return(character())
  }

  return(inputs %>% pull(1))
}


#' extract all reactives from shiny server code
#'
#' @description extract all reactives using regex from shiny server code
#'
#' @param code \code{string} server.R code
#' @param blocks \code{data.frame} of all code blocks in server.R code
#' @param regex \code{string} regex pattern to detect reactive inputs
#'
#' @import dplyr stringr
extract_reactives <- function(code, blocks, regex = regex_pattern()$input_pattern) {

  reactives <- blocks %>%
    filter(str_detect(type, fixed("reactive", ignore_case=TRUE)) | str_detect(type, fixed("module", ignore_case=TRUE)), type != "reactiveValues") %>%
    pull(name)

  if(reactives %>% length()) {
    reactives <- reactives %>%
      paste0("()")
  }

  reactive_value <- blocks %>%
    filter(type == "reactiveValues") %>%
    pull(name)

  if(reactive_value %>% length()) {
    reactives <- code %>%
      str_match_all_perl(paste0(reactive_value, regex)) %>%
      pull(1) %>%
      unique() %>%
      c(reactives)
  }

  return(reactives)
}


#' extract from within code block expressions
#'
#' @description extract and concat isolate bodies from code block expression
#'
#' @param code \code{string} expression of code block
#' @param type \code{string} type of code block (observe, reactive, observeEvent, ...)
#' @param regex \code{string} regex pattern to detect isolation blocks
#'
#' @import stringr dplyr
extract_isolates <- function(code, type, regex = regex_pattern()$isolate) {
  if(type %in% c("eventReactive", "observeEvent")) {
    return(code)
  }

  isolates <- code %>%
    str_match_all_perl(regex)

  if(nrow(isolates) == 0) {
    return("")
  }

  isolates %>%
    magrittr::set_colnames(c("match", "name", "isolate")) %>%
    pull(isolate) %>%
    glue::glue_collapse(sep=", ")
}


#' delete isolate block from within code block expressions
#'
#' @description detect and delete isolate blocks from code block expression
#'
#' @param code \code{string} expression of code block
#' @param type \code{string} type of code block (observe, reactive, observeEvent, ...)
#' @param regex \code{string} regex pattern to detect isolation blocks
#'
#' @importFrom stats setNames
#' @import stringr dplyr
delete_isolates <- function(code, type, regex = regex_pattern()$isolate) {
  if(type %in% c("eventReactive", "observeEvent")) {
    return("{}")
  }

  isolates <- code %>%
    str_match_all_perl(regex)

  if(nrow(isolates) == 0) {
    return(code)
  }

  isolates <- isolates %>%
    magrittr::set_colnames(c("match", "name", "isolate"))

  code <-
    str_replace_all(
      code,
      setNames(fixed("\n"), isolates$match)
    )
}


#' extract all inputs from server alongside ids
#'
#' @description get all inputs with assigned ids from parsed server code
#'
#' @param server \code{data.frame} of all code blocks in server.R code
#'
#' @import dplyr
extract_inputs_with_id <- function(server) {
  inputs <- server %>%
    select(name = inputs) %>%
    tidyr::unnest(name) %>%
    distinct() %>%
    mutate(
      id = paste(id = paste("input",row_number(), sep = "_")),
      type = "input"
    )
}


#' extract all reactiveValues from server alongside ids
#'
#' @description get all reactiveValues with assigned ids from parsed server code
#'
#' @param server \code{data.frame} of all code blocks in server.R code
#'
#' @import dplyr
extract_reactive_values_with_id <- function(server) {

  reactive_value <- server %>%
    filter(type == "reactiveValues") %>%
    pull(name)

  if(identical(reactive_value, character())) {
    return(character())
  }

  reactives <- server %>%
    select(name = reactives) %>%
    tidyr::unnest(name) %>%
    filter(str_detect(name, paste0(reactive_value,"\\$"))) %>%  # filter on reactiveValues, ignore reactive functions, and reactiveVals
    distinct() %>%
    mutate(
      id = paste(id = paste("reactiveValue",row_number(), sep = "_")),
      type = "reactiveValue"
    )
}


#' extract all reactive srouces from server alongside ids
#'
#' @description get all reactive sources with assigned ids from parsed server code
#'
#' @param app \code{data.frame} of all code blocks, inputs and reatives with ids in server.R code
#'
#' @import dplyr
extract_sources_with_id <- function(app) {
  inputs <- extract_inputs_with_id(app)
  reactive_values <- extract_reactive_values_with_id(app)
  blocks <- app %>% select(id, name, type)

  sources <- bind_rows(inputs, reactive_values, blocks)
}


