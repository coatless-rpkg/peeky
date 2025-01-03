# Test create_standalone_shinylive_app() ----

test_that("create_standalone_shinylive_app(): creates object with correct structure", {
    # Create sample app data representing a simple R Shiny application
    app_data <- list(
        list(
            name = "app.R",
            content = "library(shiny)\n...",
            type = "text"
        )
    )

    # Create standalone app object with the sample data
    result <- create_standalone_shinylive_app(
        app_data = app_data,                  # List containing file data
        output_dir = "test_dir",              # Directory where files will be written
        url = "https://example.com/app.json"  # Source URL of the application
    )

    # Verify the object has the correct S3 class
    testthat::expect_s3_class(result, "standalone_shinylive_app")

    # Check if all components match the input data
    testthat::expect_equal(result$files, app_data)                            # Files should match input
    testthat::expect_equal(result$output_dir, "test_dir")                     # Output dir should match
    testthat::expect_equal(result$source_url, "https://example.com/app.json") # URL should match
})

# Test print.standalone_shinylive_app() ----

test_that("print.standalone_shinylive_app(): displays correct output for R app", {
    # Create temporary directory for test files
    temp_dir <- base::tempfile()
    base::dir.create(temp_dir)

    # Ensure cleanup of temporary directory after test
    base::on.exit(base::unlink(temp_dir, recursive = TRUE))

    # Create sample R Shiny app data
    app_data <- list(
        list(
            name = "app.R",
            content = "library(shiny)\n...",
            type = "text"
        )
    )

    # Create standalone app object
    obj <- create_standalone_shinylive_app(app_data, temp_dir, "https://example.com")

    # Capture both standard output and messages from print
    output <- base::c(
        utils::capture.output(base::print(obj), type = "output"),
        utils::capture.output(base::print(obj), type = "message")
    )

    # Combine output lines into single string for pattern matching
    output_str <- base::paste(output, collapse = "\n")

    # Verify output contains expected headers and app type
    testthat::expect_match(output_str, "Standalone Shinylive Application")
    testthat::expect_match(output_str, "Type: R Shiny")
})

test_that("print.standalone_shinylive_app(): displays correct output for Python app", {
    # Create temporary directory for test files
    temp_dir <- base::tempfile()
    base::dir.create(temp_dir)

    # Ensure cleanup of temporary directory after test
    base::on.exit(base::unlink(temp_dir, recursive = TRUE))

    # Create sample Python Shiny app data
    app_data <- list(
        list(
            name = "app.py",
            content = "from shiny import App\n...",
            type = "text"
        )
    )

    # Create standalone app object
    obj <- create_standalone_shinylive_app(app_data, temp_dir, "https://example.com")

    # Capture both standard output and messages from print
    output <- base::c(
        utils::capture.output(base::print(obj), type = "output"),
        utils::capture.output(base::print(obj), type = "message")
    )

    # Combine output lines into single string for pattern matching
    output_str <- base::paste(output, collapse = "\n")

    # Verify output correctly identifies Python app type
    testthat::expect_match(output_str, "Type: Python Shiny")
})
