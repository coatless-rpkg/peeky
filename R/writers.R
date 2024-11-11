#' Write Shinylive Applications to a Quarto Document
#'
#' Converts a list of parsed Shinylive applications into a single Quarto document.
#' Creates a properly formatted .qmd file with YAML frontmatter, organized sections
#' for each application, and correctly formatted code blocks with all necessary
#' markers and options.
#'
#' @param apps List of parsed Shinylive applications. Each application should
#'   contain:
#'   - `engine`: Character string identifying the app type (`"r"` or `"python"`)
#'   - `options`: List of YAML-style options from the original code block
#'   - `files`: Named list of file definitions, each containing:
#'     - `name`: Character string of the file name
#'     - `content`: Character string of the file content
#'     - `type`: Character string indicating the file type
#'
#' @param qmd_path Character string. Path where the Quarto document should be
#'   written. Should end with `.qmd` extension. Parent directory will be created
#'   if it doesn't exist.
#'
#' @section Document Structure:
#'
#' Creates a Quarto document with this structure:
#' ````markdown
#' ---
#' title: Extracted Shinylive Applications
#' filters:
#'   - shinylive
#' ---
#'
#' # Shinylive Applications
#'
#' ## Application 1
#'
#' ```{shinylive-r}
#' #| viewerHeight: 500
#' ## file: app.R
#' ## type: text
#' library(shiny)
#' ...
#' ```
#'
#' ## Application 2
#' ...
#' ````
#'
#' @section Option Formatting:
#' Options are converted to YAML format based on their type:
#' - Logical: `#| option: true` or `#| option: false`
#' - Numeric: `#| option: 500`
#' - Character:
#'   - Single: `#| option: "value"`
#'   - Vector: `#| option: ["value1", "value2"]`
#'
#' @details
#' The function performs these steps:
#'
#' 1. Creates YAML frontmatter with required Quarto settings
#' 2. For each application:
#'    - Adds a section header with application number
#'    - Creates a code block with appropriate engine (`shinylive-r`/`shinylive-python`)
#'    - Converts and adds all application options
#'    - Adds file markers and content for each file
#'    - Properly closes the code block
#' 3. Writes the complete document to the specified path
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' # Example apps list structure
#' apps <- list(
#'   list(
#'     engine = "r",
#'     options = list(
#'       viewerHeight = 500,
#'       fullWidth = TRUE
#'     ),
#'     files = list(
#'       "app.R" = list(
#'         name = "app.R",
#'         content = "library(shiny)\n...",
#'         type = "text"
#'       )
#'     )
#'   )
#' )
#'
#' write_apps_to_quarto(apps, "applications.qmd")
#' }
#'
#' @seealso
#' * [write_apps_to_dirs()] for alternative directory output format
write_apps_to_quarto <- function(apps, qmd_path) {
    # Initialize Quarto content with YAML header
    quarto_content <- c(
        "---",
        "title: Extracted Shinylive Applications",
        "filters:",
        "  - shinylive",
        "---",
        "",
        "# Shinylive Applications",
        ""
    )

    # Add each app as a code block
    for (i in seq_along(apps)) {
        app <- apps[[i]]

        # Add section header for this app
        quarto_content <- c(
            quarto_content,
            sprintf("## Application %d", i),
            ""
        )

        # Start code block with engine and options
        block_header <- sprintf("```{shinylive-%s}", app$engine)
        if (length(app$options) > 0) {
            # Convert options to YAML style
            options_yaml <- sapply(names(app$options), function(key) {
                value <- app$options[[key]]
                if (is.logical(value)) {
                    sprintf("#| %s: %s", key, tolower(as.character(value)))
                } else if (is.numeric(value)) {
                    sprintf("#| %s: %d", key, value)
                } else if (is.character(value)) {
                    if (length(value) > 1) {
                        # Handle character vectors
                        sprintf("#| %s: [%s]", key, paste(sprintf('"%s"', value), collapse = ", "))
                    } else {
                        sprintf('#| %s: "%s"', key, value)
                    }
                }
            })
            block_header <- c(block_header, options_yaml)
        }

        quarto_content <- c(quarto_content, block_header)

        # Add file markers and content for each file
        for (file_name in names(app$files)) {
            file_data <- app$files[[file_name]]
            quarto_content <- c(
                quarto_content,
                sprintf("## file: %s", file_name),
                if (!is.null(file_data$type)) sprintf("## type: %s", file_data$type),
                file_data$content
            )
        }

        # Close code block and add spacing
        quarto_content <- c(quarto_content, "```", "", "")
    }

    # Write to file
    writeLines(quarto_content, qmd_path)
}


#' Write Multiple Shinylive Applications to Separate Directories
#'
#' Takes a list of parsed Shinylive applications and writes each to its own
#' numbered subdirectory. Creates consistently numbered directories with proper
#' padding (e.g., `app_01`, `app_02`) and preserves all application files and
#' metadata.
#'
#' @param apps List of parsed Shinylive applications. Each application should
#'   contain:
#'   - `engine`: Character string identifying the app type (`"r"` or `"python"`)
#'   - `options`: List of YAML-style options from the original code block
#'   - `files`: Named list of file definitions, each containing:
#'     - `name`: Character string of the file name
#'     - `content`: Character string of the file content
#'     - `type`: Character string indicating the file type
#'
#' @param base_dir Character string. Base directory where application
#'   subdirectories should be created. Will be created if it doesn't exist.
#'
#' @details
#' The function performs these steps:
#'
#' 1. Creates the base directory if needed
#' 2. Calculates proper padding for subdirectory numbers
#' 3. For each application:
#'    - Creates a padded, numbered subdirectory (e.g., `app_01`, `app_02`)
#'    - Writes all application files, preserving directory structure
#'    - Creates a metadata JSON file with engine and options info
#'
#' @section Directory Structure:
#' Creates a directory structure like:
#'
#' ```sh
#' base_dir/
#'   ├── app_01/
#'   │   ├── app.R
#'   │   ├── data/
#'   │   │   └── example.csv
#'   │   └── shinylive_metadata.json
#'   ├── app_02/
#'   │   ├── app.py
#'   │   └── shinylive_metadata.json
#'   └── ...
#' ```
#'
#' @section Metadata File:
#'
#' Each directory includes a `shinylive_metadata.json` file containing:
#'
#' ```json
#' {
#'   "engine": "r",
#'   "options": {
#'     "viewerHeight": 500,
#'     "...": "..."
#'   }
#' }
#' ```
#'
#' @seealso
#' - [padding_width()] for directory number padding calculation
#' - [write_apps_to_quarto()] for alternative Quarto output format
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' # Example apps list structure
#' apps <- list(
#'   list(
#'     engine = "r",
#'     options = list(viewerHeight = 500),
#'     files = list(
#'       "app.R" = list(
#'         name = "app.R",
#'         content = "library(shiny)\n...",
#'         type = "text"
#'       )
#'     )
#'   ),
#'   list(
#'     engine = "python",
#'     options = list(),
#'     files = list(
#'       "app.py" = list(
#'         name = "app.py",
#'         content = "from shiny import App\n...",
#'         type = "text"
#'       )
#'     )
#'   )
#' )
#'
#' write_apps_to_dirs(apps, "extracted_apps")
#' }
write_apps_to_dirs <- function(apps, base_dir) {
    fs::dir_create(base_dir)


    # Calculate padding width based on number of apps
    number_padding <- padding_width(length(apps))
    dir_format <- sprintf("app_%%0%dd", number_padding)

    # Process each app
    for (i in seq_along(apps)) {
        app <- apps[[i]]

        # Create numbered subdirectory using dynamic padding
        app_dir <- file.path(base_dir, sprintf(dir_format, i))
        fs::dir_create(app_dir)

        # Write each file in the app
        for (file_name in names(app$files)) {
            file_data <- app$files[[file_name]]
            file_path <- file.path(app_dir, file_name)

            # Ensure parent directory exists
            fs::dir_create(dirname(file_path))

            # Write file content
            writeLines(file_data$content, file_path)
        }

        # Write metadata
        metadata <- list(
            engine = app$engine,
            options = app$options
        )
        jsonlite::write_json(
            metadata,
            file.path(app_dir, "shinylive_metadata.json"),
            auto_unbox = TRUE,
            pretty = TRUE
        )
    }
}

#' Write Standalone Shinylive Application Files from JSON Data
#'
#' Extracts files from parsed Shinylive `app.json` data and writes them to a
#' specified directory. Creates a standalone application object containing
#' metadata and commands for running the application.
#'
#' @param json_data List. Parsed JSON data from a Shinylive `app.json` file.
#'   Each element should be a list containing:
#'   - `name`: Character string of the file name
#'   - `content`: Character string of the file content
#'   - `type`: Character string indicating the file type
#'
#' @param source_url Character string. The original URL from which the `app.json`
#'   was downloaded. Used for reference and provenance tracking in the returned
#'   object.
#'
#' @param output_dir Character string. Directory where application files should
#'   be extracted. Defaults to `"converted_shiny_app"`. Will be created if it
#'   doesn't exist. Existing files in this directory may be overwritten.
#'
#' @return
#'  An object of class `"standalone_shinylive_app"` containing:
#'
#'   - `files`: List of extracted files and their metadata
#'   - `output_dir`: Path to the directory containing extracted files
#'   - `source_url`: Original URL of the application
#'
#' @details
#' The function performs these steps:
#'
#' 1. Creates the output directory if it doesn't exist
#' 2. Iterates through each file in the JSON data
#' 3. Writes each file to the output directory, preserving names
#' 4. Creates a standalone application object with metadata
#'
#' File paths are created relative to the output directory. Parent
#' directories in file paths will be created as needed.
#'
#' @section File Structure:
#' Expected JSON data structure:
#' ```
#' [
#'   {
#'     "name": "app.R",
#'     "content": "library(shiny)\n...",
#'     "type": "text"
#'   },
#'   {
#'     "name": "data/example.csv",
#'     "content": "x,y\n1,2\n...",
#'     "type": "text"
#'   }
#' ]
#' ```
#'
#'
#' @seealso
#' - [create_standalone_shinylive_app()] for object creation
#' - [validate_app_json()] for JSON data validation
#'
#' @examples
#' \dontrun{
#' # Example JSON data structure
#' json_data <- list(
#'   list(
#'     name = "app.R",
#'     content = "library(shiny)\nui <- fluidPage()\n...",
#'     type = "text"
#'   ),
#'   list(
#'     name = "data.csv",
#'     content = "col1,col2\n1,2\n3,4",
#'     type = "text"
#'   )
#' )
#'
#' app <- write_standalone_shinylive_app(
#'   json_data,
#'   "https://example.com/app.json",
#'   "my_app"
#' )
#' }
#' @keywords internal
write_standalone_shinylive_app <- function(json_data, source_url, output_dir = "converted_shiny_app") {
    # Create output directory
    fs::dir_create(output_dir)

    # Extract files
    for (file in json_data) {
        file_path <- file.path(output_dir, file$name)
        writeLines(file$content, file_path)
    }

    # Return standalone command object
    create_standalone_shinylive_app(json_data, output_dir, source_url)
}
