#' Validate Shinylive `app.json` Structure
#'
#' Validates that a parsed `app.json` structure meets the requirements for a
#' Shinylive application. Checks for proper list structure, required fields,
#' and non-empty content. Provides detailed error messages when validation fails.
#'
#' @param json_data List. Parsed JSON data from an app.json file. Should be a list
#'   where each element represents a file and contains:
#'   - `name`: Character string of the file name
#'   - `content`: Character string of the file content
#'   - `type`: Character string indicating the file type
#'
#' @return Logical TRUE if validation passes. If validation fails, throws an error
#'   with detailed information about the validation failure using cli_abort().
#'
#' @section Validation Rules:
#' The function checks these requirements:
#' 1. `json_data` must be a list
#' 2. `json_data` must contain at least one element
#' 3. Each element must be a list (representing a file)
#' 4. Each file list must contain all required fields:
#'    - `name`
#'    - `content`
#'    - `type`
#'
#' @section Error Messages:
#'
#' The function provides detailed error messages for these cases:
#'
#' - Not a list: "Expected a list or array of files"
#' - Empty list: "File list is empty"
#' - Invalid file entry: "Each entry must be a file object"
#' - Missing fields: Lists specific missing required fields
#'
#'
#' @section Expected JSON Structure:
#' The expected JSON structure is an array of objects, where each object represents
#' a file in the application.
#'
#' ```json
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
#' @seealso
#' - [find_shinylive_app_json()] which uses this validation
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Valid structure
#' valid_data <- list(
#'   list(
#'     name = "app.R",
#'     content = "library(shiny)\n...",
#'     type = "text"
#'   ),
#'   list(
#'     name = "data.csv",
#'     content = "x,y\n1,2",
#'     type = "text"
#'   )
#' )
#' validate_app_json(valid_data)  # Returns TRUE
#'
#' # Invalid structures that will error:
#' validate_app_json(list())  # Empty list
#' validate_app_json(list(
#'   list(name = "app.R")  # Missing required fields
#' ))
#' }
validate_app_json <- function(json_data) {
    if (!is.list(json_data)) {
        cli::cli_abort(c(
            "Invalid app.json structure",
            "x" = "Expected a list or array of files",
            "i" = "The app.json file should contain an array of objects, each representing a file"
        ))
    }

    if (length(json_data) == 0) {
        cli::cli_abort(c(
            "Invalid app.json structure",
            "x" = "File list is empty",
            "i" = "The app.json file must contain at least one file"
        ))
    }

    # Check if it's an array of file objects
    required_fields <- c("name", "content", "type")
    for (file in json_data) {
        if (!is.list(file)) {
            cli::cli_abort(c(
                "Invalid app.json structure",
                "x" = "Each entry must be a file object",
                "i" = "Every item in app.json should be a list with {.field name}, {.field content}, and {.field type} fields"
            ))
        }
        missing_fields <- setdiff(required_fields, names(file))
        if (length(missing_fields) > 0) {
            cli::cli_abort(c(
                "Invalid app.json structure",
                "x" = "Missing required fields: {.field {missing_fields}}",
                "i" = "Each file object must contain {.field name}, {.field content}, and {.field type}"
            ))
        }
    }

    TRUE
}

#' Calculate number of digits needed for padding
#'
#' @param n Number to determine digits for
#'
#' @return
#' Number of digits needed
#'
#' @keywords internal
padding_width <- function(n) {
    if (n <= 0) return(1)
    floor(log10(n)) + 1
}

