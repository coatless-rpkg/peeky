% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quarto-cell-parser.R
\name{parse_code_block}
\alias{parse_code_block}
\title{Parse a Single Shinylive Code Block}
\usage{
parse_code_block(code_text, engine)
}
\arguments{
\item{code_text}{Character string. The raw text content of a Shinylive code block,
which may contain YAML-style options, file markers, and file content.}

\item{engine}{Character string. The type of Shinylive application, either \code{"r"} or
\code{"python"}. Determines default file extension when no explicit file is specified.}
}
\value{
A list with three components:
\itemize{
\item \code{engine}: Character string indicating the application type (\code{"r"} or \code{"python"})
\item \code{options}: List of parsed YAML-style options from block headers
\item \code{files}: Named list of file definitions, where each file contains:
\itemize{
\item \code{name}: Character string of the file name
\item \code{content}: Character string of the file content
\item \code{type}: Character string indicating the file type (defaults to `"text"``)
}
}
}
\description{
Parses the content of a Shinylive code block, extracting YAML-style options,
file definitions, and content. Handles both single-file and multi-file
applications for R and Python.
}
\section{Code Block Structure}{


The code block can contain several types of lines:
\itemize{
\item \strong{YAML-style options:} Lines starting with `'#|'``
\item \strong{File markers:} Lines starting with \code{'## file:'}
\item \strong{Type markers:} Lines starting with \code{'## type:'}
\item \strong{Content:} All other non-empty lines
}

For single-file applications with no explicit file marker, the content is
automatically placed in:
\itemize{
\item \code{"app.R"} for R applications
\item \code{"app.py"} for Python applications
}
}

\examples{
\dontrun{
# Single-file R application
code <- '
#| viewerHeight: 500
library(shiny)
ui <- fluidPage()
server <- function(input, output) {}
shinyApp(ui, server)
'
result1 <- parse_code_block(code, "r")

# Multi-file Python application
code <- '
#| fullWidth: true
## file: app.py
from shiny import App, ui
app = App(app_ui)
## file: requirements.txt
## type: text
shiny>=0.5.0
'
result2 <- parse_code_block(code, "python")
}

}
\seealso{
\itemize{
\item \code{\link[=parse_yaml_options]{parse_yaml_options()}} for YAML-style option parsing
\item \code{\link[=find_shinylive_code]{find_shinylive_code()}} for extracting code blocks from HTML
}
}
\keyword{internal}
