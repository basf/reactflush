# reactflush <a href='https://github.com/svdwoude/reactflush'><img src='inst/media/reactflush.png' align="right" height="139" /></a>

[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)

<br>

## Overview

[Shiny](https://github.com/rstudio/shiny)  is an R package from RStudio that makes 
it incredibly easy to build interactive web applications with R using a reactive 
programming model. The complexity of the reactive elements within a shiny application 
can quickly become complicated and difficult to keep an overview of the sources 
and endpoints of your elements. 
`reactflush` parses your code, analyses the sources and endpoints for each element
and provides a visual overview of how all the reactive elements interact wiht one
another.

This package aims to be a complementary tool for [reactlog](https://github.com/rstudio/reactlog)
which provides a visual debugger for shiny reactivity. The key difference is that 
`reactflush` gives you an overview of your complete app based on your code instead
of logging the reactive activity while using the app. This ensures a complete overview
of all reactive elements in your app. This will help in identifying which elements
are reactive bottlenecks or how changing one element will affect other reactive elements.

## Features

- visualise the reactivity network of your shiny app
- focus one reactivity network around one single element in your app
- capable of identifying and processing source statements within your code
- identify all common elements: `input`, `reactive`,  `reactiveValues`, `reactiveVal`, `eventReactive`, `callModule`, `observe`, `observeEvent`, `render*`

![nodes](inst/media/reactflush_nodes.png)

![nodes](inst/media/reactflush_edges.png)


## Installation

``` r
# install.packages("devtools")
devtools::install_github("svdwoude/reactflush")
```
## Usage

### Complete shiny app
```r
# select directory of shiny app 
dir <- fs::path(system.file("examples", package="shiny"), "07_widgets")
# create reactflush overview
reactflush(dir)
```

![nodes](inst/media/reactflush_example_full.png)


### Focus on one element

```r
# select directory of shiny app 
dir <- fs::path(system.file("examples", package="shiny"), "07_widgets")
# select element to focus on
element <- "output$view"
# create reactflush overview
reactflush(dir, focus = element)
```

![nodes](inst/media/reactflush_example_focus.png)


## Rstudio Addin

### Complete shiny app
![rstudioa_addin_demo](inst/media/reactflush_rstudioaddin.gif)

### Focus on one element
![rstudioa_addin_demo](inst/media/reactflush_rstudioaddin_focus.gif)


