#' Download and Extract Shinylive Applications from URLs
#'
#' Downloads and extracts Shinylive applications from various URL sources. The function
#' can handle both standalone Shinylive applications and Quarto documents containing
#' embedded Shinylive applications. It automatically detects the application type
#' and extracts the necessary files.
#'
#' @param url Character string. URL pointing to one of:
#'   - A standalone Shinylive application (containing `app.json`)
#'   - A directory containing `app.json`
#'   - A Quarto document with embedded Shinylive applications
#' @param output_dir Character string. Directory where the application files should
#'   be extracted. Defaults to `"converted_shiny_app"`. Will be created if it doesn't exist.
#'
#' @return
#' An object containing the extracted application information:
#'
#'   - For standalone apps: Object of class `"standalone_shinylive_app"`
#'   - For Quarto documents: Object of class `"quarto_shinylive_apps"`
#'
#' Both object types implement custom print methods that display:
#'
#'   - Application type (R or Python)
#'   - Commands to run the application
#'   - List of extracted files
#'   - Output directory location
#'
#' @details
#' The function follows these steps:
#'
#' 1. Downloads and analyzes the content at the provided URL
#' 2. Determines if the content is a Quarto document or standalone application
#' 3. For Quarto documents:
#'    - Extracts all embedded Shinylive applications
#'    - Creates separate directories for each application
#' 4. For standalone applications:
#'    - Locates and validates the `app.json` file
#'    - Extracts all application files to the specified directory
#'
#' @section URL Resolution:
#'
#' The function attempts several strategies to find app.json:
#'
#' - Direct use of the provided URL
#' - Appending `"app.json"` to the URL
#' - Checking the parent directory
#'
#' @section Error Handling:
#'
#' The function will error with informative messages if:
#'
#' - The URL cannot be accessed
#' - No valid Shinylive application is found
#' - The `app.json` structure is invalid
#'
#' @seealso
#'
#' - [peek_quarto_shinylive_app()] for handling Quarto documents specifically
#' - [peek_standalone_shinylive_app()] for handling standalone applications
#'
#' @export
#' @examplesIf interactive()
#' # Download a standalone Shinylive application
#' url <- "https://tutorials.thecoatlessprofessor.com/convert-shiny-app-r-shinylive/"
#'
#' app <- peek_shinylive_app(url)
#'
#' # Extract to a specific directory
#' app <- peek_shinylive_app(
#'   url,
#'   output_dir = "my_extracted_app"
#' )
#'
#' # Download from a Quarto document
#' apps <- peek_shinylive_app("https://quarto-ext.github.io/shinylive/")
peek_shinylive_app <- function(url, output_dir = "converted_shiny_app") {
    # Download content
    resp <- httr::GET(url)
    if (httr::http_error(resp)) {
        cli::cli_abort(c(
            "Failed to download from URL",
            "x" = "Could not access {.url {url}}",
            "i" = "Check that the URL is accessible and you have internet access."
        ))
    }

    # Check content type
    content_type <- httr::headers(resp)[["content-type"]]

    # If HTML, determine if it's a Quarto document
    if (grepl("text/html", content_type, fixed = TRUE)) {
        html_content <- httr::content(resp, "text", encoding = "UTF-8")
        doc <- rvest::read_html(html_content)

        # Check if it's a Quarto document (has main.content and quarto-specific elements)
        is_quarto <- length(doc |>
                                rvest::html_elements("main.content#quarto-document-content")) > 0

        if (is_quarto) {
            return(peek_quarto_shinylive_app(url, output_format = "app-dir", output_path = output_dir))
        }
    }

    # Find valid app.json URL and data
    app_json_result <- find_shinylive_app_json(url)
    if (!app_json_result$valid) {
        cli::cli_abort(c(
            "Could not find valid Shinylive application",
            "x" = "No Shinylive app found at {.url {url}}",
            "i" = "Check that the URL points to either a Shinylive app.json file or a directory containing app.json"
        ))
    }

    # Use the already parsed data
    write_standalone_shinylive_app(app_json_result$data, app_json_result$url, output_dir)
}

#' Extract Shinylive Applications from Quarto Documents
#'
#' Downloads a Quarto document and extracts all embedded Shinylive applications.
#' Applications can be extracted either as separate directories (for individual use)
#' or combined into a new Quarto document (for documentation). The function handles
#' both R and Python Shinylive applications.
#'
#' @param url Character string. URL of the Quarto document containing Shinylive
#'   applications. The document should contain code blocks with class
#'   `'shinylive-r'` or `'shinylive-python'`.
#'
#' @param output_format Character string. Determines how the applications should be
#'   extracted. Must be one of:
#'   - `"app-dir"`: Creates separate directories for each application
#'   - `"quarto"`: Combines all applications into a single Quarto document
#'
#' @param output_path Character string or NULL. Where to write the extracted
#'   applications. If NULL, uses default paths:
#'   - For "app-dir": "./converted_shiny_apps/"
#'   - For "quarto": "./converted_shiny_apps.qmd"
#'
#' @return
#' An object of class `"shinylive_commands"` that provides:
#'
#'   - Pretty-printed instructions via cli
#'   - Commands to run each extracted application
#'   - Information about output locations
#'   - Setup instructions for Quarto documents (if applicable)
#'
#' @section Output Formats:
#' The two output formats serve different purposes:
#'
#' - `"app-dir"`:
#'   - Creates numbered directories (app_1, app_2, etc.)
#'   - Each directory contains a complete, runnable application
#'   - Includes metadata about the original application
#'   - Best for running or modifying individual applications
#'
#' - `"quarto"`:
#'   - Creates a single .qmd file containing all applications
#'   - Preserves original YAML options and file structure
#'   - Adds necessary Quarto configuration
#'   - Best for documentation or sharing multiple applications
#'
#' @section Error Handling:
#'
#' The function will error with informative messages if:
#'
#' - The URL cannot be accessed
#' - No Shinylive applications are found in the document
#' - The document structure is invalid
#'
#' @seealso
#'
#' - [find_shinylive_code()] for the code block extraction
#' - [write_apps_to_dirs()] for directory output format
#' - [write_apps_to_quarto()] for Quarto document output format
#'
#' @export
#' @examplesIf interactive()
#' # Extract as separate applications
#' result <- peek_quarto_shinylive_app(
#'   "https://quarto-ext.github.io/shinylive",
#'   output_format = "app-dir"
#' )
#'
#' # Combine into a new Quarto document
#' result <- peek_quarto_shinylive_app(
#'   "https://quarto-ext.github.io/shinylive",
#'   output_format = "quarto",
#'   output_path = "my_apps.qmd"
#' )
#'
#' # Print will show instructions for running the apps
#' print(result)
peek_quarto_shinylive_app <- function(url,
                                      output_format = c("app-dir", "quarto"),
                                      output_path = NULL) {
    # Validate output format
    output_format <- match.arg(output_format)

    # Set default output path if not provided
    if (is.null(output_path)) {
        output_path <- if (output_format == "app-dir") {
            "converted_shiny_apps"
        } else {
            "converted_shiny_apps.qmd"
        }
    }

    # Download and parse HTML
    resp <- httr::GET(url)
    if (httr::http_error(resp)) {
        cli::cli_abort(c(
            "Failed to download Quarto document",
            "x" = "Could not access {.url {url}}",
            "i" = "Check that the URL is correct and you have internet access."
        ))
    }

    html_content <- httr::content(resp, "text", encoding = "UTF-8")

    # Find and parse all shinylive code blocks
    apps <- find_shinylive_code(html_content)

    # Check if any apps were found
    if (length(apps) == 0) {
        cli::cli_abort(c(
            "No Shinylive applications found",
            "x" = "The Quarto document at {.url {url}} contains no Shinylive applications.",
            "i" = "Check that the document contains code blocks with class {.code shinylive-r} or {.code shinylive-python}."
        ))
    }

    # Handle output based on format
    if (output_format == "app-dir") {
        write_apps_to_dirs(apps, output_path)
    } else {
        write_apps_to_quarto(apps, output_path)
    }

    # Return command object
    create_quarto_shinylive_apps(apps, output_format, output_path)
}

#' Download and Extract a Standalone Shinylive Application
#'
#' Downloads and extracts a standalone Shinylive application from a URL. The function
#' locates the application's `app.json` file, validates its structure, and extracts
#' all application files to a local directory. Works with both R and Python
#' Shinylive applications.
#'
#' @param url Character string. URL pointing to either:
#'   - A Shinylive app.json file directly
#'   - A directory containing app.json
#'
#'   The function will automatically append `"app.json"` to directory URLs.
#'
#' @param output_dir Character string. Directory where the application files
#'   should be extracted. Defaults to `"converted_shiny_app"`. Will be created
#'   if it doesn't exist. If the directory already exists, files may be
#'   overwritten.
#'
#' @return
#' An object of class `"standalone_shinylive_app"` containing:
#'
#'   - List of extracted files and their contents
#'   - Source URL of the application
#'   - Output directory location
#'
#' The object has a custom print method that displays:
#'
#'   - Application type (R or Python)
#'   - Command to run the application
#'   - List of extracted files by type
#'   - File locations
#'
#' @section File Structure:
#'
#' A valid Shinylive application should have an app.json file containing:
#'
#' - At least one application file (e.g., `app.R` or `app.py`)
#' - Optional supporting files (e.g., data files, `requirements.txt`)
#' - File metadata including name, content, and type
#'
#' @section Error Handling:
#'
#' The function will error with informative messages if:
#'
#' - No `app.json` file is found at the URL
#' - The `app.json` file has invalid structure
#' - The `app.json` file cannot be downloaded
#' - Required application files are missing
#'
#' @seealso
#' - [find_shinylive_app_json()] for `app.json` validation
#' - [write_standalone_shinylive_app()] for file extraction
#' - [peek_shinylive_app()] for a more general-purpose download function
#'
#' @export
#' @examplesIf interactive()
#'
#' # Download from a direct app.json URL
#' app <- peek_standalone_shinylive_app(
#'   "https://tutorials.thecoatlessprofessor.com/convert-shiny-app-r-shinylive/app.json"
#' )
#'
#' # Download from a directory URL (app.json will be appended)
#' app <- peek_standalone_shinylive_app(
#'   "https://tutorials.thecoatlessprofessor.com/convert-shiny-app-r-shinylive/",
#'   output_dir = "my_local_app"
#' )
#'
#' # Print shows how to run the application
#' print(app)
peek_standalone_shinylive_app <- function(url, output_dir = "converted_shiny_app") {

    # Find and validate URL for app.json
    # we append app.json to the URL as one of the possible paths
    app_json_result <- find_shinylive_app_json(url)
    if (!app_json_result$valid) {
        cli::cli_abort(c(
            "Failed to find valid app.json",
            "x" = "No app.json found at {.url {url}}",
            "i" = "The URL should point to a directory containing a valid Shinylive app.json file"
        ))
    }

    # Parse the already validated data
    write_standalone_shinylive_app(app_json_result$data, app_json_result$url, output_dir)
}
