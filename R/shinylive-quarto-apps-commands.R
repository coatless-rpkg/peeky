#' Create a `quarto_shinylive_apps` class object
#'
#' @param apps List of parsed apps
#' @param output_format The format used (`"app-dir"` or `"quarto"`)
#' @param output_path Path where apps were written
#'
#' @return
#' Object of class `"quarto_shinylive_apps"`
#'
#' @keywords internal
create_quarto_shinylive_apps <- function(apps, output_format, output_path) {
    structure(
        list(
            apps = apps,
            output_format = output_format,
            output_path = output_path
        ),
        class = "quarto_shinylive_apps"
    )
}

#' Print method for `quarto_shinylive_apps` objects
#'
#' @param x Object of class `"quarto_shinylive_apps"`
#' @param ... Additional arguments passed to print
#'
#' @export
print.quarto_shinylive_apps <- function(x, ...) {

    # Style definitions
    path_style <- list(color = "cyan")
    cmd_style <- list(color = "yellow")


    if (x$output_format == "app-dir") {
        # Group apps by engine
        r_apps <- which(sapply(x$apps, function(x) x$engine == "r"))
        py_apps <- which(sapply(x$apps, function(x) x$engine == "python"))

        # Get full normalized path for base directory
        full_base_path <- normalizePath(x$output_path, winslash = "/", mustWork = TRUE)

        cli::cli_h1("Shinylive Applications")

        # Print R apps
        if (length(r_apps) > 0) {
            cli::cli_h2("Shiny for {.emph R} Applications")
            cli::cli_text("Run in {.emph R}:")
            for (app_num in r_apps) {
                dir_path <- file.path(full_base_path, sprintf("app_%d", app_num))
                cli::cli_code(
                    sprintf('shiny::runApp("%s")', dir_path),
                    language = "r"
                )
            }
        }

        # Print Python apps
        if (length(py_apps) > 0) {
            if (length(r_apps) > 0) cli::cli_text() # Add spacing
            cli::cli_h2("Shiny for {.emph Python} Applications")
            cli::cli_text("Run in {.emph Terminal}:")
            for (app_num in py_apps) {
                app <- x$apps[[app_num]]
                dir_path <- file.path(full_base_path, sprintf("app_%d", app_num))

                # Find main Python file
                file_names <- names(app$files)
                main_py_file <- if ("app.py" %in% file_names) {
                    "app.py"
                } else {
                    # Take first .py file if app.py not found
                    py_files <- file_names[grepl("\\.py$", file_names)]
                    if (length(py_files) > 0) py_files[1] else ""
                }

                app_path <- file.path(dir_path, main_py_file)
                cli::cli_code(
                    sprintf('shiny run --reload --launch-browser "%s"', app_path),
                    language = "bash"
                )
            }
        }
    } else {
        # Quarto format
        cli::cli_h1("Quarto Document with Shinylive Applications")

        # Get the directory path and check if it's different from working directory
        doc_dir <- dirname(x$output_path)
        doc_name <- basename(x$output_path)
        needs_cd <- !identical(normalizePath(doc_dir), normalizePath(getwd()))

        cli::cli_h2("Setup and Preview Steps")

        # Calculate step numbers based on whether cd is needed
        current_step <- 1

        # Step: Change directory (only if needed)
        if (needs_cd) {
            cli::cli_text()
            cli::cli_text("{.strong Step {current_step}:} Change to the document directory:")
            cli::cli_code('cd "{doc_dir}"', language = "bash")
            current_step <- current_step + 1
        }

        # Step: Install extension
        cli::cli_text()
        cli::cli_text("{.strong Step {current_step}:} Install the Shinylive extension:")
        cli::cli_code("quarto add quarto-ext/shinylive", language = "bash")
        current_step <- current_step + 1

        # Step: Preview document
        cli::cli_text()
        cli::cli_text("{.strong Step {current_step}:} Preview the document:")
        cli::cli_code('quarto preview "{doc_name}"', language = "bash")

        # Add summary of contained apps
        cli::cli_text()
        cli::cli_h2("Contents")
        r_count <- sum(sapply(x$apps, function(x) x$engine == "r"))
        py_count <- sum(sapply(x$apps, function(x) x$engine == "python"))

        cli::cli_bullets(c(
            "*" = "R applications: {r_count}",
            "*" = "Python applications: {py_count}"
        ))
    }

    invisible(x)
}
