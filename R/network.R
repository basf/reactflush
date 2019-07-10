#' compute source, target dependency network
#'
#' @description create a source, target data.frame listing all dependencies and their direction
#'
#' @param app \code{data.frame} all annotated code blocks in server.R code
#' @param type \code{string} which type to annotate: inputs or reactives
#' @param relation \code{string} which type to relation: dependency or isolate
#'
#' @import dplyr stringr
compute_dependency_network <- function(app, type = c("inputs","reactives"), relation = c("dependency","isolate")) {
  type <- rlang::arg_match(type)
  relation <- rlang::arg_match(relation)
  rel <- ifelse(relation == "dependency", "deps", "islt")

  type_deps <- sep_(type, rel)

  # get all sources, ready to join with dependencies
  sources <- extract_sources_with_id(app) %>%
    select(source_id = id, source = name)

  app %>%
    select(target_type = type, target = name, target_id = id,  source = !! type_deps) %>%
    tidyr::unnest(source) %>%
    mutate(
      target = ifelse(target == "", paste(target_type, source, sep="::"), target),
      source = gsub("\\(\\)","", source),
      # strip last character of type argument
      source_type = str_sub(type, end = -2),
      relation = relation
    ) %>%
    left_join(sources, by = "source")
}


#' compute source, target side effects network
#'
#' @description create a source, target data.frame listing all side effects and their direction
#'
#' @param app \code{data.frame} all annotated code blocks in server.R code
#'
#' @import dplyr stringr
compute_side_effect_network <- function(app) {
  # get the name for inputs
  input <- app %>%
    pull(input) %>%
    unique()

  targets <- extract_sources_with_id(app) %>%
    select(target_id = id, target = name)

  # get the name for reactiveValues
  reactive_value <- app %>%
    filter(type == "reactiveValues") %>%
    pull(name)

  # extract list of side_effects
  side_effects <- app %>%
    select(source_type = type, source = name, source_id = id, target = side_effects_deps) %>%
    tidyr::unnest(target)

  # if no side-effects are found, we can return the empty tibble
  if(nrow(side_effects) == 0) {
    return(side_effects)
  }

  side_effects <- side_effects %>%
    mutate(
      target = str_trim(gsub("\\(\\)","", target)),
      target_type = ifelse(any(str_detect(target, paste0(reactive_value,"\\$"))), "reactiveValue", "reactiveVal"),
      relation = "side_effect"
    ) %>%
    left_join(targets, by = "target")

  return(side_effects)
}


#' compute source, target network of (isolated) dependencies and side effects
#'
#' @description create a source, target data.frame listing all dependencies and side effects and their direction
#'
#' @param app \code{data.frame} all annotated code blocks in server.R code
#'
#' @import dplyr stringr
compute_network <- function(app) {
  network <- bind_rows(
    app %>% compute_dependency_network(type = "inputs", relation = "dependency"),
    app %>% compute_dependency_network(type = "inputs", relation = "isolate"),
    app %>% compute_dependency_network(type = "reactives", relation = "dependency"),
    app %>% compute_dependency_network(type = "reactives", relation = "isolate"),
    app %>% compute_side_effect_network()
  ) %>% select(
    source_id, source, source_type,
    target_id, target, target_type,
    relation
  ) %>%
  arrange(target_type, target, relation, source_type, source)

  return(network)
}
