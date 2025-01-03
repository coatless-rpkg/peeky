# Test create_quarto_shinylive_apps() ----

test_that("create_quarto_shinylive_apps(): creates object with correct structure", {
    # Create sample apps list with an R Shiny application
    apps <- list(
        list(
            engine = "r",                           # Specify R as the engine
            options = list(viewerHeight = 500),     # Set viewer height option
            files = list(
                app.R = list(
                    name = "app.R",                 # Main R app file
                    content = "library(shiny)\n...", # Sample content
                    type = "text"                   # File type
                )
            )
        )
    )

    # Create Quarto Shinylive apps object
    result <- create_quarto_shinylive_apps(
        apps = apps,                    # List of apps
        output_format = "app-dir",      # Output as directory structure
        output_path = "test_path"       # Base path for output
    )

    # Verify object structure
    testthat::expect_s3_class(result, "quarto_shinylive_apps") # Check class
    testthat::expect_equal(result$apps, apps)                  # Check apps list
    testthat::expect_equal(result$output_format, "app-dir")    # Check format
    testthat::expect_equal(result$output_path, "test_path")    # Check path
})

# Test print.quarto_shinylive_apps() ----

test_that("print.quarto_shinylive_apps(): displays correct output for app-dir format", {
    # Create temporary directory for test files
    temp_dir <- base::tempfile()
    base::dir.create(temp_dir)
    base::on.exit(base::unlink(temp_dir, recursive = TRUE))

    # Create sample apps list with both R and Python apps
    apps <- list(
        # R Shiny app
        list(
            engine = "r",
            options = list(),
            files = list(app.R = list(name = "app.R", content = "", type = "text"))
        ),
        # Python Shiny app
        list(
            engine = "python",
            options = list(),
            files = list(app.py = list(name = "app.py", content = "", type = "text"))
        )
    )

    # Create object for testing
    obj <- create_quarto_shinylive_apps(apps, "app-dir", temp_dir)

    # Capture all output (both stdout and stderr for CLI messages)
    output <- base::c(
        utils::capture.output(base::print(obj), type = "output"),
        utils::capture.output(base::print(obj), type = "message")
    )

    # Combine output for pattern matching
    output_str <- base::paste(output, collapse = "\n")

    # Verify output contains expected sections
    testthat::expect_match(output_str, "Shinylive Applications")         # Main header
    testthat::expect_match(output_str, "Shiny for R Applications")       # R section
    testthat::expect_match(output_str, "Shiny for Python Applications")  # Python section
})

test_that("print.quarto_shinylive_apps(): displays correct output for quarto format", {
    # Create temporary directory for test files
    temp_dir <- base::tempfile()
    base::dir.create(temp_dir)
    base::on.exit(base::unlink(temp_dir, recursive = TRUE))

    # Create sample apps list with R app
    apps <- list(
        list(
            engine = "r",
            options = list(),
            files = list(app.R = list(name = "app.R", content = "", type = "text"))
        )
    )

    # Create object with quarto format
    obj <- create_quarto_shinylive_apps(
        apps,
        "quarto",                                      # Quarto document format
        base::file.path(temp_dir, "test.qmd")         # Output as .qmd file
    )

    # Capture all output
    output <- base::c(
        utils::capture.output(base::print(obj), type = "output"),
        utils::capture.output(base::print(obj), type = "message")
    )
    output_str <- base::paste(output, collapse = "\n")

    # Verify Quarto-specific output
    testthat::expect_match(output_str, "Quarto Document with Shinylive Applications") # Main header
    testthat::expect_match(output_str, "Setup and Preview Steps")                     # Setup instructions
})
