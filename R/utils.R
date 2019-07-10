#' convert named list to data.frame
#'
#' @param listfordf names list to be converted to data.frame
#' @param name column name of names
#' @param value column name of values
list_to_df <- function(listfordf, name = "name", value = "value"){
  if(!is.list(listfordf)) stop("it should be a list")

  df <- list(value = listfordf)
  class(df) <- c("tbl_df", "data.frame")
  attr(df, "row.names") <- .set_row_names(length(listfordf))

  if (!is.null(names(listfordf))) {
    df$name <- names(listfordf)
  }

  df %>%
    as_tibble() %>%
    mutate(value = as.character(value)) %>%
    select(!!name := name, !!value := value)
}

#' colors for mermaid graph nodes
#'
#' @param name column name of names
#' @param value column name of colors
mermaid_colors <- function(name = "name", value = "color"){
  colors <- list(
    # Inputs "red"
    input     = "#c50022",
    # Reactives "blue"
    reactive  = "#21a0d2",
    reactiveVal = "#21a0d2",
    reactiveValue = "#21a0d2",
    eventReactive = "#004a96",
    callModule = "#004a96",
    # render "green"
    render    = "#65ac1e",
    renderUI  = "#00793a",
    downloadHandler = "#00793a",
    # observe "orange"
    observe   = "#f39500",
    observeEvent   = "#f39500"
  )

  list_to_df(colors, name, value)
}

#' check if directory has shiny files: app.R or server.R
#' @param dir directory to look for shiny app
dir_contains_app <- function(dir) {
  fs::dir_ls(dir) %>% fs::path_file() %in% c("app.R", "server.R") %>% any()
}

#' stringr match_all adaptation to work with PCRE engine
#' @description Helper function to replicate str_match_all functionality with
#' PERL like syntax
#' @param s string
#' @param p pattern
#' @param as_tibble return matches as tibble
#'
#' @import stringr
str_match_all_perl <- function(s, p, as_tibble = TRUE) {
  m <- gregexpr(enc2utf8(p), enc2utf8(s), perl=TRUE) # PCRE-NORMALIZED
  # note that normalization is needed only for regex-matching

  out <- vector("list", length(s))

  # vectorized over s
  for (j in seq_along(s))
  {
    nmatch <- length(m[[j]])
    ncapt  <- length(attr(m[[j]], "capture.names"))

    if (length(m) == 1 && m[[j]] == -1) next

    out[[j]] <- matrix(str_sub(s[[j]], m[[j]],
                               m[[j]]+attr(m[[j]], "match.length")-1),
                       nrow=nmatch, ncol=ncapt+1)

    if (ncapt > 0) {
      cs <- as.integer(attr(m[[j]], "capture.start"))
      cl <- as.integer(attr(m[[j]], "capture.length")) -1
      cl <- ifelse(cl<0,0,cl)
      out[[j]][,-1] <- str_sub(s[j], cs, cs+cl)

      if (any(str_length(attr(m[[j]], "capture.names")) > 0))
        colnames(out[[j]]) <- c("", attr(m[[j]], "capture.names"))
    }
  }

  if(as_tibble) {
    out <- out %>%
      magrittr::extract2(1) %>%
      tibble::as_tibble()
  }

  return(out)
}


#' Function to construct regex pattern
#' @description Helper function to maintain & construct regex pattern
regex_pattern <- function() {
  r <- list(
    name = "[a-zA-Z0-9\\._]+",                   # name
    list = "[a-zA-Z0-9\\._$]+",                  # list
    asgn = "\\s*(?:\\=|<-)\\s*",                 # assignment
    func = "([a-zA-Z0-9\\._\\:]+)",              # function
    args = "[a-zA-Z0-9\\._:$\\,\\(\\)\\=\\s]*",  # arguments
    inpt = "\\$[a-zA-Z0-9\\._]*",                # input
    islt = "isolate",                            # isolate
    s = list( # source
      s = "source\\(.*?",                     # source(...
      q = "[\"']",                            # quote
      p = ".*\\.R",                           # path
      v = ".*\\)[$]value.*\\n"                # ...)$value ...
    ),
    ss = list( # shiny-server
      f = "function\\(",                       # function(
      n = "([a-zA-Z0-9\\._]+)",                # argument name
      b = "(\\{(?:[^{}]*|(?4))*\\})"            # code block
    )
  )

  list(
    name = r$name,
    assignment = r$asgn,
    shiny_server = paste0(r$ss$f, r$ss$n, "\\s*,\\s*", r$ss$n, "\\s*(?:,\\s*", r$ss$n, ")?\\)\\s*", r$ss$b),
    shiny_server_blocks = paste0("(?:", "(", r$list, ")", r$asgn, ")?", "(?:(", r$name, ")\\$)?", r$func, "\\s*", regex_recursion("(",")",4, .sub = regex_recursion("{","}",5, .optional = TRUE)), "\\s*",regex_recursion("{","}",6),"*"),
    function_pattern = paste0("(","(", r$list, ")", r$asgn, "function", r$args,")", regex_recursion("{","}", 3)),
    source_pattern   = paste0(r$s$s, r$s$q, "(", r$s$p, ")", r$s$q, r$s$v),
    input_pattern = r$inpt,
    values_assgn = paste0(r$inpt,r$asgn),
    isolate = paste0("(?:", "(", r$list, ")", r$asgn, ")?", r$islt, regex_recursion("(",")",2)),
    rstudio_selection = paste0("((?:", r$name, "\\$)?",r$name,")(?:",r$asgn,")?"),
    braces_start = "\\([^()]+"
  )

  # ui Output regex ([a-zA-Z0-9\._]*[Oo]utput)\(["']([a-zA-Z0-9\._]+)["']\)

}


#' create recursive regex
#'
#' @param .open symbol at start of recursion
#' @param .close symbol at end of recursion
#' @param .group reference group within regex
#' @param .optional if match is optional
#' @param .sub optional regex sub pattern to match
regex_recursion <- function(.open, .close, .group = 1, .optional = FALSE, .sub = "") {
  regex <- paste0("(\\",.open,"(?:[^",.open,.close,"]*?",.sub,"[^",.open,.close,"]*?|(?",.group,"))*\\",.close,")")

  if(.optional) {
    regex <- paste0(regex,"?")
  }

  return(regex)
}


#' concat strings with "_" seperator
#'
#' @param ... vector of strings
sep_ <- function(...) {
  glue::glue(... , .sep = "_")
}
