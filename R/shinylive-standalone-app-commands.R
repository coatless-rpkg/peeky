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

    # Check if there are R or Python files
    is_r_app <- any(grepl("\\.R$", file_names))
    is_python_app <- any(grepl("\\.py$", file_names))

    # Find main Python file (prefer app.py, but take first .py if not found)
    main_py_file <- if (is_python_app) {
        app_py <- file_names[file_names == "app.py"]
        if (length(app_py) > 0) {
            app_py[1]
        } else {
            py_files <- file_names[grepl("\\.py$", file_names)]
            if (length(py_files) > 0) py_files[1] else NULL
        }
    } else {
        NULL
    }

    # Normalized the output path
    full_path <- normalizePath(x$output_dir, winslash = "/", mustWork = TRUE)

    cli::cli_h1("Standalone Shinylive Application")

    # Show app type and run command with appropriate syntax highlighting
    if (is_r_app) {
        cli::cli_text("Type: {.emph R} Shiny")
        cli::cli_text("Run in {.emph R}:")
        cli::cli_code(
            sprintf('shiny::runApp("%s")', full_path),
            language = "r"
        )
    } else if (is_python_app) {
        dir_path <- file.path(full_path, main_py_file)
        cli::cli_text("Type: {.emph Python} Shiny")
        cli::cli_text("Run in {.emph Terminal}:")
        cli::cli_code(
            sprintf('shiny run --reload --launch-browser "%s"', dir_path),
            language = "bash"
        )
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
    cli::cli_text("Location: {.path {full_path}}")

    invisible(x)
}
