#' Parse a Single Shinylive Code Block
#'
#' Parses the content of a Shinylive code block, extracting YAML-style options,
#' file definitions, and content. Handles both single-file and multi-file
#' applications for R and Python.
#'
#' @param code_text Character string. The raw text content of a Shinylive code block,
#'   which may contain YAML-style options, file markers, and file content.
#' @param engine Character string. The type of Shinylive application, either `"r"` or
#'   `"python"`. Determines default file extension when no explicit file is specified.
#'
#' @return
#'
#' A list with three components:
#'
#'   - `engine`: Character string indicating the application type (`"r"` or `"python"`)
#'   - `options`: List of parsed YAML-style options from block headers
#'   - `files`: Named list of file definitions, where each file contains:
#'     - `name`: Character string of the file name
#'     - `content`: Character string of the file content
#'     - `type`: Character string indicating the file type (defaults to `"text"``)
#'
#' @section Code Block Structure:
#'
#' The code block can contain several types of lines:
#'
#' - **YAML-style options:** Lines starting with `'#|'``
#' - **File markers:** Lines starting with `'## file:'`
#' - **Type markers:** Lines starting with `'## type:'`
#' - **Content:** All other non-empty lines
#'
#' For single-file applications with no explicit file marker, the content is
#' automatically placed in:
#'
#' - `"app.R"` for R applications
#' - `"app.py"` for Python applications
#'
#' @examples
#' \dontrun{
#' # Single-file R application
#' code <- '
#' #| viewerHeight: 500
#' library(shiny)
#' ui <- fluidPage()
#' server <- function(input, output) {}
#' shinyApp(ui, server)
#' '
#' result1 <- parse_code_block(code, "r")
#'
#' # Multi-file Python application
#' code <- '
#' #| fullWidth: true
#' ## file: app.py
#' from shiny import App, ui
#' app = App(app_ui)
#' ## file: requirements.txt
#' ## type: text
#' shiny>=0.5.0
#' '
#' result2 <- parse_code_block(code, "python")
#' }
#'
#' @seealso
#'
#' - [parse_yaml_options()] for YAML-style option parsing
#' - [find_shinylive_code()] for extracting code blocks from HTML
#'
#' @keywords internal
parse_code_block <- function(code_text, engine) {
    # Split into lines for processing
    lines <- strsplit(code_text, "\n")[[1]]

    # Extract YAML-style options (lines starting with #|)
    yaml_lines <- lines[grep("^\\s*#\\|", lines)]
    options <- parse_yaml_options(yaml_lines)

    # Initialize variables for file parsing
    files <- list()
    current_file <- NULL
    current_content <- character()
    current_type <- "text"

    # Process lines
    for (i in seq_along(lines)) {
        line <- lines[i]

        # Check for file marker
        if (grepl("^##\\s*file:\\s*", line)) {
            # Save previous file if it exists
            if (!is.null(current_file)) {
                files[[current_file]] <- list(
                    name = current_file,
                    content = paste(current_content, collapse = "\n"),
                    type = current_type
                )
            }

            # Start new file
            current_file <- sub("^##\\s*file:\\s*", "", line)
            current_content <- character()
            current_type <- "text"
            next
        }

        # Check for type marker
        if (grepl("^##\\s*type:\\s*", line)) {
            current_type <- sub("^##\\s*type:\\s*", "", line)
            next
        }

        # Skip YAML options
        if (grepl("^\\s*#\\|", line)) {
            next
        }

        # If we're in a file, add content
        if (!is.null(current_file)) {
            current_content <- c(current_content, line)
        } else if (!grepl("^\\s*#|^\\s*$", line)) {
            # If no file specified yet and line isn't empty or comment,
            # treat as single-file app with appropriate extension
            current_file <- if (engine == "r") "app.R" else "app.py"
            current_content <- c(current_content, line)
        }
    }

    # Save last file
    if (!is.null(current_file)) {
        files[[current_file]] <- list(
            name = current_file,
            content = paste(current_content, collapse = "\n"),
            type = current_type
        )
    }

    list(
        engine = engine,
        options = options,
        files = files
    )
}

#' Parse YAML-style Options from Shinylive Code Blocks
#'
#' Parses YAML-style configuration options from Shinylive code block headers.
#' These options appear as lines prefixed with `'#|'` and follow a simplified
#' YAML-like syntax for key-value pairs.
#'
#' @param yaml_lines Character vector. Each element should be a line containing
#'   a YAML-style option in the format `'#| key: value'`. The `'#|'` prefix will
#'   be stripped during processing.
#'
#' @return
#' A named list of parsed options where:
#'
#'   - Array values (e.g., `'[1, 2, 3]'`) are converted to character vectors
#'   - Boolean values ('true'/'false') are converted to logical values
#'   - Numeric values are converted to numeric type
#'   - Other values remain as character strings
#'
#' @details
#' The function handles several value types:
#'
#'   - **Arrays:** Values in the format `'[item1, item2, ...]'`
#'   - **Booleans:** Values 'true' or 'false'
#'   - **Numbers:** Integer values
#'   - **Strings:** All other values
#'
#' Lines that don't contain a colon (`':'`) are ignored.
#'
#' @examples
#' \dontrun{
#' # Parse various types of options
#' yaml_lines <- c(
#'   "#| viewerHeight: 500",
#'   "#| components: [slider,button]",
#'   "#| fullWidth: true",
#'   "#| title: My App"
#' )
#' options <- parse_yaml_options(yaml_lines)
#' # Results in:
#' # list(
#' #   viewerHeight = 500,
#' #   components = c("slider", "button"),
#' #   fullWidth = TRUE,
#' #   title = "My App"
#' # )
#' }
#'
#' @seealso parse_code_block
#'
#' @keywords internal
parse_yaml_options <- function(yaml_lines) {
    # Remove #| prefix and any leading/trailing whitespace
    yaml_lines <- sub("^\\s*#\\|\\s*", "", yaml_lines)

    # Split each line into key-value pairs
    options <- list()
    for (line in yaml_lines) {
        if (grepl(":", line)) {
            parts <- strsplit(line, ":\\s*")[[1]]
            key <- parts[1]
            value <- parts[2]

            # Parse value (handle arrays, booleans, numbers)
            if (grepl("^\\[.*\\]$", value)) {
                # Array value
                value <- strsplit(gsub("\\[|\\]|\\s", "", value), ",")[[1]]
            } else if (value %in% c("true", "false")) {
                # Boolean value
                value <- value == "true"
            } else if (grepl("^\\d+$", value)) {
                # Numeric value
                value <- as.numeric(value)
            }

            options[[key]] <- value
        }
    }

    options
}



