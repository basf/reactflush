% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse.R
\name{parse_options}
\alias{parse_options}
\title{extract options from code shiny server code blocks}
\usage{
parse_options(body, expr)
}
\arguments{
\item{body}{\code{string} body of code block}

\item{expr}{\code{string} the expression within the code block}
}
\description{
extract and concat options from code block
  \code{\( <options> \{ <expr>\} <options>\) } by simple substraction of the
  expression from the code block
}
