## Resubmission

This is a resubmission. In response to the CRAN review (2025-01-17), I have
made the following changes:

* Software, package, and API names ('shinylive', 'shiny', 'quarto') are written
  in single quotes in the Title and Description.

* There are no references describing methods to cite: the package is a tool for
  retrieving and extracting the source of published 'shinylive' applications.

* Added `\value` tags documenting the return value for the exported print
  methods (`print.quarto_shinylive_apps()`,
  `print.standalone_shinylive_app()`), describing the returned object (class)
  and the side effects.

* Removed the examples from the unexported (internal) helper functions
  (`find_shinylive_app_json()`, `find_shinylive_code()`, `parse_code_block()`,
  `parse_yaml_options()`, `validate_app_json()`).

* Removed all remaining `\dontrun{}`. The examples for the exported `peek_*()`
  functions download live applications from the internet, so they are wrapped
  with `@examplesIf interactive()`.

* The writing functions no longer use a default output path. The output
  directory / file is now a required argument, so the package never writes to
  the user's working directory (or anywhere in the user's file space) unless
  the caller explicitly requests it. All examples, tests, and the vignette
  write to `tempdir()` (or are non-executable display-only code blocks).

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

* The NOTE reports two URLs as possibly invalid:
  <https://pyodide.org/en/stable/> and
  <https://pyodide.org/en/stable/usage/packages-in-pyodide.html>.
  Both return HTTP 429 (Too Many Requests) to the automated checker but are
  valid and load correctly in a browser; the 429 is transient rate limiting.
