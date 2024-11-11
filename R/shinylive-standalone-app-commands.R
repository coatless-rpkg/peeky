#' Create a `standalone_shinylive_app` class object
#'
#' @param app_data      Data of the app
#' @param output_dir    Path where app is written
#' @param url           The location of where the app was downloaded from
#'
#' @return
#' Object of class `"standalone_shinylive_app"`
#'
#' @keywords internal
create_standalone_shinylive_app <- function(app_data, output_dir, url) {
    structure(
        list(
            files = app_data,
            output_dir = output_dir,
            source_url = url
        ),
        class = "standalone_shinylive_app"
    )
}

#' Print method for `standalone_shinylive_app` objects
#'
#' @param x Object of class `"standalone_shinylive_app"`
#' @param ... Additional arguments passed to print
#'
#' @export
print.standalone_shinylive_app <- function(x, ...) {
    # First detect if it's an R or Python app by looking at file extensions
    files_list <- if(is.list(x$files[[1]])) x$files else list(x$files)
    file_names <- sapply(files_list, function(f) f$name)

    is_r_app <- any(grepl("\\.R$", file_names))
    is_python_app <- any(grepl("\\.py$", file_names))

    cli::cli_h1("Standalone Shinylive Application")

    # Show app type and run command
    if (is_r_app) {
        cli::cli_text("Type: {.emph R Shiny}")
        cli::cli_text("Run in {.emph R}:")
        cli::cli_code(sprintf('shiny::runApp("%s")', x$output_dir))
    } else if (is_python_app) {
        cli::cli_text("Type: {.emph Python Shiny}")
        cli::cli_text("Run in {.emph Terminal}:")
        cli::cli_code(sprintf('shiny run --reload --launch-browser "%s"', x$output_dir))
    }

    # Add contents section
    cli::cli_text()
    cli::cli_h2("Contents")

    # Group files by extension
    extensions <- tools::file_ext(file_names)
    extensions[extensions == ""] <- "no extension"
    files_by_ext <- split(file_names, extensions)

    # List files by extension
    for (ext in sort(names(files_by_ext))) {
        files <- files_by_ext[[ext]]
        if (ext == "no extension") {
            cli::cli_text("{.strong Files without extension}:")
        } else {
            cli::cli_text("{.strong .{ext} files}:")
        }
        cli::cli_ul(files)
    }

    # Add total file count
    cli::cli_text()
    cli::cli_text("Total files: {length(files_list)}")

    # Add location information
    cli::cli_text()
    cli::cli_text("Location: {.file {x$output_dir}}")

    invisible(x)
}
