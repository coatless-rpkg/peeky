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

        cli::cli_h1("Shinylive Applications")

        # Print R apps
        if (length(r_apps) > 0) {
            cli::cli_h2("R Applications")
            cli::cli_text("Run in {.emph R}:")
            for (app_num in r_apps) {
                dir_path <- sprintf("%s/app_%d", x$output_path, app_num)
                cli::cli_code(sprintf('shiny::runApp("%s")', dir_path))
            }
        }

        # Print Python apps
        if (length(py_apps) > 0) {
            if (length(r_apps) > 0) cli::cli_text() # Add spacing
            cli::cli_h2("Python Applications")
            cli::cli_text("Run in {.emph Terminal}:")
            for (app_num in py_apps) {
                dir_path <- sprintf("%s/app_%d", x$output_path, app_num)
                cli::cli_code(sprintf('shiny run --reload --launch-browser "%s"', dir_path))
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
            cli::cli_code(sprintf('cd "%s"', doc_dir))
            current_step <- current_step + 1
        }

        # Step: Install extension
        cli::cli_text()
        cli::cli_text("{.strong Step {current_step}:} Install the Shinylive extension:")
        cli::cli_code("quarto add quarto-ext/shinylive")
        current_step <- current_step + 1

        # Step: Preview document
        cli::cli_text()
        cli::cli_text("{.strong Step {current_step}:} Preview the document:")
        cli::cli_code(sprintf('quarto preview "%s"', doc_name))

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

    # Add invisible return
    invisible(x)
}
