#' generate mermaid code from app code analysis
#'
#' @param app \code{data.frame} containing all code blocks, inputs and reactives
#' @param network \code{data.frame} containing all reactive dependencies
#' @param focus \code{string} reactive node to focus on, all it's dependencies on blocks which
#'   depend on it will be shown
#' @param focus_field \code{enum} what the type of the focus argument is:
#'   \itemize{
#'     \item \code{name}: assigned name of reactive block (\code{(output$)<name> <- reactive/render*})
#'     \item \code{id}: assigned id by \code{\link{parse_code_blocks}}
#'     \item \code{type}: type of block (reactive, observeEvent, ...)
#'   }
#'
#' @import dplyr purrr stringr
#' @export
render_mermaid <- function(app, network, focus, focus_field = c("name","id","type")){
  focus_field <- rlang::arg_match(focus_field)
  if(focus_field == "name") {
    postfix <- character()
  } else {
    postfix <- focus_field
  }

  mermaid_header <- "graph LR;"

  if(!missing(focus)) {
    s <- paste(c("source",postfix), collapse = "_")
    t <- paste(c("target",postfix), collapse = "_")

    network <- network %>%
      filter(!! sym(s) == focus | !! sym(t) == focus)
  }

  edges <- network %>%
    mutate(
      relation_id = row_number() - 1,
      mermaid = pmap(list(source_id, target_id, relation_id, relation), convert_to_mermaid_connection)
    )

  mermaid_edges <- edges %>%
    pull(mermaid) %>%
    unique()

  nodes <- extract_sources_with_id(app) %>%
    filter(type != "reactiveValues")

  if(!missing(focus)) {
    nodes_attached <- edges %>% pull(source_id) %>%
      c(edges %>% pull(target_id)) %>%
      unique()

    nodes <- nodes %>%
      filter(id %in% nodes_attached)
  }

  nodes <- nodes %>%
    mutate(
      name = gsub("^.*\\$", "", name),
      type_c = ifelse(str_detect(type, "render") & type != "renderUI", "render", type)
    ) %>%
    left_join(mermaid_colors(name = "type_c", value = "color"), by = "type_c") %>%
    mutate(
      mermaid = pmap(list(id, name, type, color), convert_to_mermaid_node)
    )

  mermaid_nodes <- nodes %>%
    pull(mermaid) %>%
    unique()

  mermaid <- glue::glue_collapse(c(mermaid_header, mermaid_nodes, mermaid_edges))

  return(mermaid)
}


#' generate mermaid code for single reactive block
#'
#' @param id \code{string} id of block as assigned by \code{\link{parse_code_blocks}}
#' @param name \code{string} assigned name of block (\code{(output$)<name> <- reactive/render*})
#' @param type \code{string} type of block (reactive, observeEvent, ...)
#' @param color \code{string} color block should get in mermaid graph
#'
#' @export
convert_to_mermaid_node <- function(id, name, type, color) {
  # label should show the name of the node and possibly its type if that would not be obvious from the color
  if(type %in% c("input", "reactive")) {
    label <- name
  } else if(name == "") {
    label <- type
  } else {
    label <- paste0("<code>",type,"</code> - ", name)
  }

  node <- glue::glue("{id}({label})")
  style <- glue::glue("style {id} fill:{color}, fill-opacity:0.4, stroke:{color}, stroke-width:2px")

  mermaid <- glue::glue("{node}; {style};")

  return(mermaid)
}


#' generate mermaid code for single dependency
#'
#' @param source id of source as assigned by \code{\link{parse_code_blocks}}
#' @param target id of target as assigned by \code{\link{parse_code_blocks}}
#' @param relation \code{string} type of relation ("isolate" or "dependency")
#'
#' @export
convert_to_mermaid_connection <- function(source, target, id, relation) {

  line <- switch(relation,
           isolate = "-.->",
           side_effect = "-->",
           dependency = "-->")

  color <- switch(relation,
                 isolate = "#999999",
                 side_effect = "black",
                 dependency = "black")

  dash <- switch(relation,
                  isolate = "10",
                  side_effect = "2",
                  dependency = "none")

  connection <- glue::glue("{source}{line}{target}")
  style <- glue::glue("linkStyle {id} stroke:{color}, stroke-width:1px, fill:none, stroke-dasharray:{dash}")

  mermaid <- glue::glue("{connection}; {style};")

  return(mermaid)
}


#' plot mermaid diagram from analyzed shiny app code
#'
#' @param app \code{data.frame} containing all code blocks, inputs and reactives
#' @param network \code{data.frame} containing all reactive dependencies
#' @param ... optional arguments for \code{\link{render_mermaid}}
#'
#' @import DiagrammeR
#' @export
plot_mermaid <- function(app, network, ...) {
  mermaid_code <- render_mermaid(app, network, ...)
  DiagrammeR::mermaid(mermaid_code)

}
