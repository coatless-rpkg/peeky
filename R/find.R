#' Find and Validate Shinylive app.json
#'
#' Attempts to locate and validate a Shinylive app.json file from a given base URL.
#' The function tries multiple possible paths and validates both the HTTP response
#' and JSON structure.
#'
#' @param base_url Character string. The base URL to search for app.json.
#'
#' @return
#' A list with three components:
#'
#'   - `valid` Logical indicating if a valid app.json was found
#'   - `url` Character string of the successful URL, or NULL if not found
#'   - `data` List containing the parsed JSON data if valid, NULL otherwise
#'
#' @details
#' The function performs the following steps:
#'
#' 1. Generates three possible paths to check:
#'    - The base URL as provided
#'    - base URL + `"/app.json"``
#'    - Parent directory + `"/app.json"`
#' 2. For each path:
#'    - Attempts an HTTP GET request
#'    - Verifies the content type is JSON
#'    - Parses and validates the JSON structure
#'    - Returns immediately if valid app.json is found
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Direct app.json URL
#' result <- find_shinylive_app_json("https://example.com/app.json")
#'
#' # Directory containing app.json
#' result <- find_shinylive_app_json("https://example.com/myapp/")
#'
#' # Check if valid
#' if (result$valid) {
#'   cat("Found app.json at:", result$url)
#' }
#' }
find_shinylive_app_json <- function(base_url) {
    # List of possible paths to try
    possible_paths <- c(
        base_url,
        file.path(base_url, "app.json"),
        file.path(dirname(base_url), "app.json")
    )

    # Try each path
    for (path in possible_paths) {
        tryCatch({
            resp <- httr::GET(path)

            # If this is already JSON content, verify it's a valid app.json
            if (grepl("application/json", httr::headers(resp)[["content-type"]], fixed = TRUE)) {
                content <- httr::content(resp, "text")
                # Try to parse as JSON and validate structure
                json_data <- jsonlite::fromJSON(content, simplifyDataFrame = FALSE)
                if (validate_app_json(json_data)) {
                    return(list(
                        valid = TRUE,
                        url = path,
                        data = json_data  # Return parsed data instead of raw content
                    ))
                }
            }
        }, error = function(e) NULL)
    }
    return(list(valid = FALSE, url = NULL, data = NULL))
}

#' Find Shinylive Code Blocks in Quarto HTML
#'
#' Parses HTML content to extract and process Shinylive code blocks for both R and Python
#' applications. This function identifies code blocks with class `'shinylive-r'` or
#' `'shinylive-python'` and processes their content into structured application data.
#'
#' @param html Character string containing HTML content. The HTML should contain
#'   code blocks with class `'shinylive-r'` or `'shinylive-python'` to be processed.
#'
#' @return
#' A list of parsed Shinylive applications. Each list element contains:
#'
#'   - `engine`: Character string indicating the application type (`"r"` or `"python"`)
#'   - `options`: List of parsed YAML-style options from the code block
#'   - `files`: List of file definitions, where each file contains:
#'     - `name`: Character string of the file name
#'     - `content`: Character string of the file content
#'     - `type`: Character string indicating the file type
#'
#' @details
#'
#' The function performs the following steps:
#'
#' 1. Parses the HTML content using `rvest`
#' 2. Extracts code blocks with classes `'shinylive-r'` or `'shinylive-python'`
#' 3. For each code block:
#'    - Determines the engine type from the 'data-engine' attribute
#'    - Extracts the code text content
#'    - Parses the code block structure using `parse_code_block()`
#'
#' Code blocks should follow the Shinylive format with optional YAML-style
#' options (prefixed with `'#|'`) and file markers (prefixed with `'## file:'`).
#'
#' @seealso parse_code_block
#'
#' @examples
#' \dontrun{
#' html_content <- '
#' <pre class="shinylive-r" data-engine="r">
#' #| viewerHeight: 500
#' ## file: app.R
#' library(shiny)
#' ui <- fluidPage()
#' server <- function(input, output) {}
#' shinyApp(ui, server)
#' </pre>
#' '
#' apps <- find_shinylive_code(html_content)
#' }
#'
#' @keywords internal
find_shinylive_code <- function(html) {
    # Parse HTML with rvest
    doc <- rvest::read_html(html)

    # Find all shinylive code blocks (both R and Python)
    code_blocks <- doc |>
        rvest::html_elements("pre.shinylive-r, pre.shinylive-python")

    # Process each code block, using the data-engine attribute to determine the type
    lapply(code_blocks, function(block) {
        engine <- block |> rvest::html_attr("data-engine")
        code_text <- block |> rvest::html_text()

        # Parse the code block with engine information
        parse_code_block(code_text, engine)
    })
}


