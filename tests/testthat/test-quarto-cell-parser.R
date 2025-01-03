# Test parse_code_block() ----

test_that("parse_code_block(): handles different types of blocks", {
    # Test an R code block with multiple components:
    # - YAML-style options
    # - Multiple files
    # - Different content types
    r_code <- '#| viewerHeight: 500
#| standalone: true
## file: app.R
library(shiny)
ui <- fluidPage()
server <- function(input, output) {}
shinyApp(ui, server)
## file: data.csv
## type: text
x,y
1,2'

    # Parse the R code block
    r_result <- parse_code_block(r_code, "r")

    # Verify engine identification
    testthat::expect_equal(r_result$engine, "r")

    # Verify YAML options were parsed correctly
    testthat::expect_equal(r_result$options$viewerHeight, 500)
    testthat::expect_true(r_result$options$standalone)

    # Verify all expected files were identified and named correctly
    testthat::expect_true(
        base::all(c("app.R", "data.csv") %in% base::names(r_result$files))
    )

    # Test a Python code block with simpler structure:
    # - Single YAML option
    # - Single file
    # - No explicit type declaration
    py_code <- '
  #| viewerHeight: 400
  ## file: app.py
  from shiny import App
  '

    # Parse the Python code block
    py_result <- parse_code_block(py_code, "python")

    # Verify Python-specific parsing
    testthat::expect_equal(py_result$engine, "python")
    testthat::expect_equal(py_result$options$viewerHeight, 400)
    testthat::expect_true("app.py" %in% base::names(py_result$files))
})

# Test parse_yaml_options() ----

test_that("parse_yaml_options handles different value types", {
    # Test parsing of different YAML value types:
    # - Numeric (viewerHeight)
    # - Boolean (standalone)
    # - Array (components)
    # - String (layout)
    yaml_lines <- c(
        "#| viewerHeight: 500",      # Numeric value
        "#| standalone: true",       # Boolean value
        "#| components: [viewer,editor]", # Array value
        "#| layout: vertical"        # String value
    )

    # Parse the YAML options
    result <- parse_yaml_options(yaml_lines)

    # Verify numeric values are parsed correctly
    testthat::expect_equal(result$viewerHeight, 500)

    # Verify array values are split and processed correctly
    testthat::expect_equal(result$components, c("viewer", "editor"))

    # Verify boolean values are converted properly
    testthat::expect_true(result$standalone)

    # Verify string values remain as strings
    testthat::expect_equal(result$layout, "vertical")
})
