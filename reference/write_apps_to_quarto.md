# Write Shinylive Applications to a Quarto Document

Converts a list of parsed Shinylive applications into a single Quarto
document. Creates a properly formatted .qmd file with YAML frontmatter,
organized sections for each application, and correctly formatted code
blocks with all necessary markers and options.

## Usage

``` r
write_apps_to_quarto(apps, qmd_path)
```

## Arguments

- apps:

  List of parsed Shinylive applications. Each application should
  contain:

  - `engine`: Character string identifying the app type (`"r"` or
    `"python"`)

  - `options`: List of YAML-style options from the original code block

  - `files`: Named list of file definitions, each containing:

    - `name`: Character string of the file name

    - `content`: Character string of the file content

    - `type`: Character string indicating the file type

- qmd_path:

  Character string. Path where the Quarto document should be written.
  Should end with `.qmd` extension. Parent directory will be created if
  it doesn't exist.

## Value

No return value, called for its side effect of writing a Quarto document
to `qmd_path`.

## Details

The function performs these steps:

1.  Creates YAML frontmatter with required Quarto settings

2.  For each application:

    - Adds a section header with application number

    - Creates a code block with appropriate engine
      (`shinylive-r`/`shinylive-python`)

    - Converts and adds all application options

    - Adds file markers and content for each file

    - Properly closes the code block

3.  Writes the complete document to the specified path

## Document Structure

Creates a Quarto document with this structure:

    ---
    title: Extracted Shinylive Applications
    filters:
      - shinylive
    ---

    # Shinylive Applications

    ## Application 1

    ```{shinylive-r}
    #| viewerHeight: 500
    ## file: app.R
    ## type: text
    library(shiny)
    ...
    ```

    ## Application 2
    ...

## Option Formatting

Options are converted to YAML format based on their type:

- Logical: `#| option: true` or `#| option: false`

- Numeric: `#| option: 500`

- Character:

  - Single: `#| option: "value"`

  - Vector: `#| option: ["value1", "value2"]`

## See also

- [`write_apps_to_dirs()`](https://r-pkg.thecoatlessprofessor.com/peeky/reference/write_apps_to_dirs.md)
  for alternative directory output format

## Examples

``` r
# Example apps list structure
apps <- list(
  list(
    engine = "r",
    options = list(
      viewerHeight = 500,
      fullWidth = TRUE
    ),
    files = list(
      "app.R" = list(
        name = "app.R",
        content = "library(shiny)\n...",
        type = "text"
      )
    )
  )
)

write_apps_to_quarto(apps, file.path(tempdir(), "applications.qmd"))
```
