# reactflush <a href='https://github.com/svdwoude/reactflush'><img src='inst/reactflush.png' align="right" height="139" /></a>

[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![build](https://gitlab.roqs.basf.net/RawatV/reactflush/badges/master/pipeline.svg)](https://gitlab.roqs.basf.net/RawatV/reactflush/commits/master)

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

This should servers as a complementary tool for [reactlog](https://github.com/rstudio/reactlog)
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

![nodes](inst/media/reactflush_nodes.svg)

![nodes](inst/media/reactflush_edges.svg)



```mermaid
graph LR;
n1(source);
style n1 color: white, fill:none, fill-opacity:0.4, stroke:none, stroke-width:2px;
n2(endpoint);
style n2 fill:none, fill-opacity:0.4, stroke:none, stroke-width:2px;
n3(source);
style n3 color: white, fill:none, fill-opacity:0.4, stroke:none, stroke-width:2px;
n4(endpoint);
style n4 fill:none, fill-opacity:0.4, stroke:none, stroke-width:2px;
n5(source);
style n5 color: white, fill:none, fill-opacity:0.4, stroke:none, stroke-width:2px;
n6(endpoint);
style n6 fill:none, fill-opacity:0.4, stroke:none, stroke-width:2px;

n1-->|connected|n2;
 linkStyle 0 stroke:black, stroke-width:1px, fill:none, stroke-dasharray:none;
n3-->|isolated|n4;
 linkStyle 1 stroke:#999999, stroke-width:1px, fill:none, stroke-dasharray:10;
 n6-->|side effect|n5;
 linkStyle 2 stroke:black, stroke-width:1px, fill:none, stroke-dasharray:2;

```

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

![nodes](inst/media/reactflush_example_full.svg)


### Focus on one element

```r
# select directory of shiny app 
dir <- fs::path(system.file("examples", package="shiny"), "07_widgets")
# select element to focus on
element <- "output$view"
# create reactflush overview
reactflush(dir, focus = element)
```

![nodes](inst/media/reactflush_example_focus.svg)


## Rstudio Addin

### Complete shiny app
![rstudioa_addin_demo](inst/reactflush_rstudioaddin.gif)

### Focus on one element
![rstudioa_addin_demo](inst/reactflush_rstudioaddin_focus.gif)


