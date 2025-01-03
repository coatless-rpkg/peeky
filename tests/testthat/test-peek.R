# Test peek_shinylive_app() ----

test_that("peek_shinylive_app(): handles HTML content correctly", {
    # Create sample HTML content with an embedded R Shiny application
    # The content simulates a Quarto document structure with a shinylive-r code block
    html_content <- '<!DOCTYPE html><html><body><main class="content" id="quarto-document-content">
    <pre class="shinylive-r" data-engine="r">
    #| viewerHeight: 500
    ## file: app.R
    library(shiny)
    ui <- fluidPage()
    server <- function(input, output) {}
    shinyApp(ui, server)
    </pre>
  </main></body></html>'

    # Mock HTTP-related functions to simulate web requests
    testthat::local_mocked_bindings(
        # Mock GET to return HTML content with appropriate headers
        GET = function(...) base::structure(
            list(
                headers = list("content-type" = "text/html"),
                content = base::charToRaw(html_content)
            ),
            class = "response"
        ),
        # Mock http_error to always return FALSE (success)
        http_error = function(...) FALSE,
        # Mock content function to return the HTML content
        content = function(...) html_content,
        .package = "httr"
    )

    # Test the function with a sample URL
    result <- peek_shinylive_app("http://example.com")

    # Verify the result is a quarto_shinylive_apps object
    testthat::expect_s3_class(result, "quarto_shinylive_apps")
})

test_that("peek_shinylive_app(): handles app.json content correctly", {
    # Create sample JSON content representing a standalone Shiny application
    json_content <- jsonlite::toJSON(list(
        list(
            name = "app.R",
            content = "library(shiny)\n...",
            type = "text"
        )
    ))

    # Mock HTTP-related functions for JSON response
    testthat::local_mocked_bindings(
        # Mock GET to return JSON content with appropriate headers
        GET = function(...) base::structure(
            list(
                headers = list("content-type" = "application/json"),
                content = base::charToRaw(json_content)
            ),
            class = "response"
        ),
        http_error = function(...) FALSE,
        content = function(...) json_content,
        .package = "httr"
    )

    # Test the function with a URL pointing to app.json
    result <- peek_shinylive_app("http://example.com/app.json")

    # Verify the result is a standalone_shinylive_app object
    testthat::expect_s3_class(result, "standalone_shinylive_app")
})

test_that("peek_quarto_shinylive_app(): handles app-dir format correctly", {
    # Create sample HTML content with a Shiny application
    html_content <- '
    <pre class="shinylive-r" data-engine="r">
    #| viewerHeight: 500
    ## file: app.R
    library(shiny)
    ui <- fluidPage()
    server <- function(input, output) {}
    shinyApp(ui, server)
    </pre>
  '

    # Mock HTTP-related functions
    testthat::local_mocked_bindings(
        GET = function(...) base::structure(
            list(
                headers = list("content-type" = "text/html"),
                content = base::charToRaw(html_content)
            ),
            class = "response"
        ),
        http_error = function(...) FALSE,
        content = function(...) html_content,
        .package = "httr"
    )

    # Create temporary directory for output
    temp_dir <- base::tempfile()

    # Test the function with app-dir output format
    result <- peek_quarto_shinylive_app(
        "http://example.com",
        output_format = "app-dir",
        output_path = temp_dir
    )

    # Verify result type and format
    testthat::expect_s3_class(result, "quarto_shinylive_apps")
    testthat::expect_equal(result$output_format, "app-dir")

    # Clean up temporary directory
    base::unlink(temp_dir, recursive = TRUE)
})

# Test peek_standalone_shinylive_app() ----

test_that("peek_standalone_shinylive_app(): processes standalone app correctly", {
    # Create sample app.json content
    app_json <- list(
        list(
            name = "app.R",
            content = "library(shiny)\n...",
            type = "text"
        )
    )

    # Mock HTTP-related functions
    testthat::local_mocked_bindings(
        # Mock GET to return JSON content with appropriate headers
        GET = function(...) base::structure(
            list(
                headers = list("content-type" = "application/json"),
                content = base::charToRaw(jsonlite::toJSON(app_json))
            ),
            class = "response"
        ),
        http_error = function(...) FALSE,
        content = function(...) jsonlite::toJSON(app_json),
        .package = "httr"
    )

    # Create temporary directory for output
    temp_dir <- base::tempfile()

    # Test standalone app processing
    result <- peek_standalone_shinylive_app(
        "http://example.com/app.json",
        output_dir = temp_dir
    )

    # Verify result type and file creation
    testthat::expect_s3_class(result, "standalone_shinylive_app")
    testthat::expect_true(base::file.exists(base::file.path(temp_dir, "app.R")))

    # Clean up temporary directory
    base::unlink(temp_dir, recursive = TRUE)
})
