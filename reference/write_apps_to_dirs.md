# Write Multiple Shinylive Applications to Separate Directories

Takes a list of parsed Shinylive applications and writes each to its own
numbered subdirectory. Creates consistently numbered directories with
proper padding (e.g., `app_01`, `app_02`) and preserves all application
files and metadata.

## Usage

``` r
write_apps_to_dirs(apps, base_dir)
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

- base_dir:

  Character string. Base directory where application subdirectories
  should be created. Will be created if it doesn't exist.

## Value

No return value, called for its side effect of writing one application
subdirectory per app (plus a metadata file) under `base_dir`.

## Details

The function performs these steps:

1.  Creates the base directory if needed

2.  Calculates proper padding for subdirectory numbers

3.  For each application:

    - Creates a padded, numbered subdirectory (e.g., `app_01`, `app_02`)

    - Writes all application files, preserving directory structure

    - Creates a metadata JSON file with engine and options info

## Directory Structure

Creates a directory structure like:

    base_dir/
    |-- app_01/
    |   |-- app.R
    |   |-- data/
    |   |   `-- example.csv
    |   `-- shinylive_metadata.json
    |-- app_02/
    |   |-- app.py
    |   `-- shinylive_metadata.json
    `-- ...

## Metadata File

Each directory includes a `shinylive_metadata.json` file containing:

    {
      "engine": "r",
      "options": {
        "viewerHeight": 500,
        "...": "..."
      }
    }

## See also

- [`padding_width()`](https://r-pkg.thecoatlessprofessor.com/peeky/reference/padding_width.md)
  for directory number padding calculation

- [`write_apps_to_quarto()`](https://r-pkg.thecoatlessprofessor.com/peeky/reference/write_apps_to_quarto.md)
  for alternative Quarto output format

## Examples

``` r
# Example apps list structure
apps <- list(
  list(
    engine = "r",
    options = list(viewerHeight = 500),
    files = list(
      "app.R" = list(
        name = "app.R",
        content = "library(shiny)\n...",
        type = "text"
      )
    )
  ),
  list(
    engine = "python",
    options = list(),
    files = list(
      "app.py" = list(
        name = "app.py",
        content = "from shiny import App\n...",
        type = "text"
      )
    )
  )
)

write_apps_to_dirs(apps, file.path(tempdir(), "extracted_apps"))
```
