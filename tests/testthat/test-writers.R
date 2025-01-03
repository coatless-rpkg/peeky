# Test write_file_content() ----

test_that("write_file_content(): handles text content correctly", {
    # Create temporary directory for test files with cleanup
    temp_dir <- base::tempfile()
    base::dir.create(temp_dir)
    base::on.exit(base::unlink(temp_dir, recursive = TRUE))

    # Test nested path creation and UTF-8 character handling
    file_path <- base::file.path(temp_dir, "nested", "test.txt")
    # Include non-ASCII characters to test UTF-8 handling
    content <- "Hello 世界!\nSecond line"

    # Write the content to file with text type
    write_file_content(content, file_path, type = "text")

    # Verify nested directory structure was created
    testthat::expect_true(base::dir.exists(base::dirname(file_path)))
    testthat::expect_true(base::file.exists(file_path))

    # Verify content was written correctly with proper UTF-8 encoding
    testthat::expect_equal(
        base::readLines(file_path, warn = FALSE, encoding = "UTF-8"),
        base::strsplit(content, "\n")[[1]]
    )
})

test_that("write_file_content(): handles binary content correctly", {
    # Create temporary directory with cleanup
    temp_dir <- base::tempfile()
    base::dir.create(temp_dir)
    base::on.exit(base::unlink(temp_dir, recursive = TRUE))

    file_path <- base::file.path(temp_dir, "test.png")
    # Create sample base64 encoded PNG content
    content <- base::paste0(
        "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAA",
        "DUlEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg=="
    )

    # Write binary content
    write_file_content(content, file_path, type = "binary")

    # Verify file exists and has correct size
    testthat::expect_true(base::file.exists(file_path))
    testthat::expect_equal(base::file.info(file_path)$size, 70L)
})

# Test write_apps_to_quarto() ----

test_that("write_apps_to_quarto(): creates correct Quarto document", {
    # Create temporary Quarto markdown file
    temp_file <- base::tempfile(fileext = ".qmd")

    # Create sample app data structure
    apps <- list(
        list(
            engine = "r",
            options = list(viewerHeight = 500),
            files = list(
                "app.R" = list(
                    name = "app.R",
                    content = "library(shiny)\n...",
                    type = "text"
                )
            )
        )
    )

    # Write apps to Quarto document
    write_apps_to_quarto(apps, temp_file)

    # Read generated document content
    content <- base::readLines(temp_file)

    # Verify Quarto document structure
    testthat::expect_true(base::any(base::grepl("^---$", content)))                  # YAML frontmatter
    testthat::expect_true(base::any(base::grepl("shinylive", content)))              # Shinylive extension
    testthat::expect_true(base::any(base::grepl("```\\{shinylive-r\\}", content)))   # Code block syntax
    testthat::expect_true(base::any(base::grepl("#\\| viewerHeight: 500", content))) # YAML options

    # Cleanup temporary file
    base::unlink(temp_file)
})

# Test write_apps_to_dirs() ----

test_that("write_apps_to_dirs creates correct directory structure", {
    # Create temporary directory
    temp_dir <- base::tempfile()
    base::dir.create(temp_dir)

    # Create sample apps with different engines
    apps <- list(
        # R Shiny app
        list(
            engine = "r",
            options = list(viewerHeight = 500),
            files = list(
                "app.R" = list(
                    name = "app.R",
                    content = "library(shiny)\n...",
                    type = "text"
                )
            )
        ),
        # Python Shiny app
        list(
            engine = "python",
            options = list(),
            files = list(
                "app.py" = list(
                    name = "app.py",
                    content = "from shiny import App\n...",
                    type = "text"
                )
            )
        )
    )

    # Write apps to directories
    write_apps_to_dirs(apps, temp_dir)

    # Verify directory structure and files
    testthat::expect_true(base::dir.exists(base::file.path(temp_dir, "app_1")))  # First app dir
    testthat::expect_true(base::dir.exists(base::file.path(temp_dir, "app_2")))  # Second app dir
    testthat::expect_true(base::file.exists(base::file.path(temp_dir, "app_1", "app.R")))  # R file
    testthat::expect_true(base::file.exists(base::file.path(temp_dir, "app_2", "app.py"))) # Python file
    # Check metadata file
    testthat::expect_true(base::file.exists(base::file.path(temp_dir, "app_1", "shinylive_metadata.json")))

    # Cleanup
    base::unlink(temp_dir, recursive = TRUE)
})

# Test write_standalone_shinylive_app() ----

test_that("write_standalone_shinylive_app creates correct file structure", {
    # Create temporary directory
    temp_dir <- base::tempfile()

    # Create sample app data with nested structure
    json_data <- list(
        list(
            name = "app.R",
            content = "library(shiny)\n...",
            type = "text"
        ),
        list(
            name = "data/example.csv", # Nested file structure
            content = "x,y\n1,2",
            type = "text"
        )
    )

    # Write standalone app
    result <- write_standalone_shinylive_app(
        json_data,
        "https://example.com/app.json",
        temp_dir
    )

    # Verify file structure and metadata
    testthat::expect_true(base::file.exists(base::file.path(temp_dir, "app.R")))  # Main app file
    testthat::expect_true(base::file.exists(base::file.path(temp_dir, "data", "example.csv")))  # Nested data file
    testthat::expect_s3_class(result, "standalone_shinylive_app")  # Correct return type
    testthat::expect_equal(result$source_url, "https://example.com/app.json")  # Source URL preserved

    # Cleanup
    base::unlink(temp_dir, recursive = TRUE)
})
