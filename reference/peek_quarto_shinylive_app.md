# Extract Shinylive Applications from Quarto Documents

Downloads a Quarto document and extracts all embedded Shinylive
applications. Applications can be extracted either as separate
directories (for individual use) or combined into a new Quarto document
(for documentation). The function handles both R and Python Shinylive
applications.

## Usage

``` r
peek_quarto_shinylive_app(
  url,
  output_format = c("app-dir", "quarto"),
  output_path
)
```

## Arguments

- url:

  Character string. URL of the Quarto document containing Shinylive
  applications. The document should contain code blocks with class
  `'shinylive-r'` or `'shinylive-python'`.

- output_format:

  Character string. Determines how the applications should be extracted.
  Must be one of:

  - `"app-dir"`: Creates separate directories for each application

  - `"quarto"`: Combines all applications into a single Quarto document

- output_path:

  Character string. Where to write the extracted applications. A
  relative path is resolved against the current working directory; an
  absolute path is used as-is.

  - For `"app-dir"`: a directory that will hold the extracted
    applications

  - For `"quarto"`: the path of the `.qmd` file to create

## Value

An object of class `"shinylive_commands"` that provides:

- Pretty-printed instructions via cli

- Commands to run each extracted application

- Information about output locations

- Setup instructions for Quarto documents (if applicable)

## Output Formats

The two output formats serve different purposes:

- `"app-dir"`:

  - Creates numbered directories (app_1, app_2, etc.)

  - Each directory contains a complete, runnable application

  - Includes metadata about the original application

  - Best for running or modifying individual applications

- `"quarto"`:

  - Creates a single .qmd file containing all applications

  - Preserves original YAML options and file structure

  - Adds necessary Quarto configuration

  - Best for documentation or sharing multiple applications

## Error Handling

The function will error with informative messages if:

- The URL cannot be accessed

- No Shinylive applications are found in the document

- The document structure is invalid

## See also

- [`find_shinylive_code()`](https://r-pkg.thecoatlessprofessor.com/peeky/reference/find_shinylive_code.md)
  for the code block extraction

- [`write_apps_to_dirs()`](https://r-pkg.thecoatlessprofessor.com/peeky/reference/write_apps_to_dirs.md)
  for directory output format

- [`write_apps_to_quarto()`](https://r-pkg.thecoatlessprofessor.com/peeky/reference/write_apps_to_quarto.md)
  for Quarto document output format

## Examples

``` r
if (FALSE) { # interactive()
# Extract as separate application directories under a temporary directory
result <- peek_quarto_shinylive_app(
  "https://quarto-ext.github.io/shinylive",
  output_format = "app-dir",
  output_path = file.path(tempdir(), "quarto_apps")
)

# Combine into a new Quarto document
result <- peek_quarto_shinylive_app(
  "https://quarto-ext.github.io/shinylive",
  output_format = "quarto",
  output_path = file.path(tempdir(), "my_apps.qmd")
)

# Print will show instructions for running the apps
print(result)
}
```
