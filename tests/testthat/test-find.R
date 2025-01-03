# Test find_shinylive_app_json() ----

test_that("find_shinylive_app_json(): validates JSON structure correctly", {
    # Create a valid app.json structure with required fields
    valid_json <- list(
        list(
            name = "app.R",
            content = "library(shiny)\n...",
            type = "text"
        )
    )

    # Create a mock HTTP response with valid JSON content
    valid_resp <- base::structure(
        list(
            headers = list("content-type" = "application/json"),
            content = base::charToRaw(jsonlite::toJSON(valid_json))
        ),
        class = "response"
    )

    # Mock httr functions to simulate successful API response
    testthat::local_mocked_bindings(
        GET = function(...) valid_resp,
        content = function(...) jsonlite::toJSON(valid_json),
        .package = "httr"
    )

    # Test with valid JSON
    result <- find_shinylive_app_json("http://example.com/app.json")

    # Verify successful validation
    testthat::expect_true(result$valid)
    testthat::expect_equal(result$url, "http://example.com/app.json")
    testthat::expect_equal(result$data, valid_json)

    # Create a mock response with invalid JSON (empty object)
    invalid_resp <- base::structure(
        list(
            headers = list("content-type" = "application/json"),
            content = base::charToRaw("{}")
        ),
        class = "response"
    )

    # Mock httr functions to simulate invalid JSON response
    testthat::local_mocked_bindings(
        GET = function(...) invalid_resp,
        content = function(...) "{}",
        .package = "httr"
    )

    # Test with invalid JSON
    result <- find_shinylive_app_json("http://example.com/app.json")

    # Verify failed validation
    testthat::expect_false(result$valid)
    testthat::expect_null(result$url)
    testthat::expect_null(result$data)
})

# Test find_shinylive_code() ----

test_that("find_shinylive_code(): extracts code blocks correctly", {
    # Create HTML content containing both R and Python Shinylive code blocks
    # Note the different structures and options in each block
    html_content <- '
  <pre class="shinylive-r" data-engine="r">
  #| viewerHeight: 500
  #| standalone: true
  ## file: app.R
  library(shiny)
  ui <- fluidPage()
  server <- function(input, output) {}
  shinyApp(ui, server)
  </pre>
  <pre class="shinylive-python" data-engine="python">
  #| standalone: true
  ## file: app.py
  from shiny import App, ui
  app = App(app_ui)
  </pre>
  '

    # Parse the HTML content to find code blocks
    result <- find_shinylive_code(html_content)

    # Verify number of code blocks found
    testthat::expect_equal(base::length(result), 2)

    # Verify correct engine identification for each block
    testthat::expect_equal(result[[1]]$engine, "r")
    testthat::expect_equal(result[[2]]$engine, "python")

    # Verify R code block structure and options
    testthat::expect_true("viewerHeight" %in% base::names(result[[1]]$options))
    testthat::expect_equal(result[[1]]$options$viewerHeight, 500)
    testthat::expect_true("app.R" %in% base::names(result[[1]]$files))

    # Verify Python code block structure and options
    testthat::expect_true("standalone" %in% base::names(result[[2]]$options))
    testthat::expect_true(result[[2]]$options$standalone)
    testthat::expect_true("app.py" %in% base::names(result[[2]]$files))
})
