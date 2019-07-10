#' anlalyze your shiny app code and display reactivity network
#'
#' @param dir root directory of shiny app
#' @param ... optional arguments for \code{\link{render_mermaid}}
#'
#' @examples
#' reactflush(fs::path(system.file("examples", package="shiny"), "01_hello"))
#' reactflush(fs::path(system.file("examples", package="shiny"), "02_text"))
#' reactflush(fs::path(system.file("examples", package="shiny"), "03_reactivity"))
#' reactflush(fs::path(system.file("examples", package="shiny"), "04_mpg"))
#' reactflush(fs::path(system.file("examples", package="shiny"), "05_sliders"))
#' reactflush(fs::path(system.file("examples", package="shiny"), "06_tabsets"))
#' reactflush(fs::path(system.file("examples", package="shiny"), "07_widgets"))
#' reactflush(fs::path(system.file("examples", package="shiny"), "08_html"))
#' reactflush(fs::path(system.file("examples", package="shiny"), "09_upload"))
#' reactflush(fs::path(system.file("examples", package="shiny"), "10_download"))
#' reactflush(fs::path(system.file("examples", package="shiny"), "11_timer"))
#'
#' @export
reactflush <- function(dir = getwd(), ...) {
  app <- parse_shiny_app(dir)
  network <- compute_network(app)

  plot_mermaid(app, network, ...)
}

#' anlalyze your shiny app code and output reactivity network to markdown
#'
#' @param dir root directory of shiny app
#'
#' @export
reactflush_markdown <- function(dir = getwd()) {
  app <- parse_shiny_app(dir)
  network <- compute_network(app)

  mermaid <- render_mermaid(app, network)

  md <- glue::glue("# reactflush \n \n ```mermaid \n {mermaid} \n ``` \n")

  readr::write_file(md,fs::path(dir,"reactlfush.md"))
}

#' RStudio addin reactflush wrapper function
reactflush_addin <- function() {
  current_file_context <- rstudioapi::getSourceEditorContext()
  current_file <- current_file_context$path
  current_dir <- fs::path_dir(current_file)
  current_file_name <- fs::path_file(current_file)

  project_dir <- rstudioapi::getActiveProject()

  if(dir_contains_app(current_dir)) {
    # go ahead with reactflush on current_dir
    reactflush_addin_helper(current_dir)

  } else if (dir_contains_app(project_dir)) {
    # go ahead with reactflush on current project dir
    reactflush_addin_helper(project_dir)

  } else{
    # we could go one level upp or down to check if that folder
    # contains a shiny app
    # stop("We can't find a shiny app \n - not in the folder of your currently open editor file \n - not in your current active project folder")
    selected_dir <- rstudioapi::selectDirectory(caption = "Select directory of Shiny app", label = "Run reactflush")
    reactflush(selected_dir)
  }
}

#' RStudio addin reactflush with selction focus wrapper function
#'
#' @importFrom magrittr "%>%"
reactflush_focus_addin <- function() {
  current_file_context <- rstudioapi::getSourceEditorContext()
  current_file <- current_file_context$path
  current_dir <- fs::path_dir(current_file)
  current_file_name <- fs::path_file(current_file)

  selection <- rstudioapi::primary_selection(current_file_context)$text

  regex <- regex_pattern()$rstudio_selection

  focus <- selection %>%
    stringr::str_match(regex) %>%
    .[,2]


  if(!dir_contains_app(current_dir)) {
    rstudioapi::showDialog("Your currently active selection is not part of a shiny app")
    return()
  }

  if(is.na(focus)) {
    rstudioapi::showDialog("We couldn't extract a valid selection. Are you sure you selected
                            the full name of an input, output or reactive function?")
    return()
  }

  reactflush_addin_helper(current_dir, focus = focus)
}

#' RStudio addin reactflush wrapper
#'
#' @importFrom magrittr "%>%"
reactflush_addin_helper <- function(dir, focus = NA) {
  message("Found shiny app in ", dir)
  if(is.na(focus)) {
    reactflush(dir) %>% print()
  } else {
    message("Foucssing on ", focus)
    reactflush(dir, focus = focus) %>% print()
  }

}
