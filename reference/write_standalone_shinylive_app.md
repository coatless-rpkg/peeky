# Write Standalone Shinylive Application Files from JSON Data

Extracts files from parsed Shinylive `app.json` data and writes them to
a specified directory. Creates a standalone application object
containing metadata and commands for running the application.

## Usage

``` r
write_standalone_shinylive_app(json_data, source_url, output_dir)
```

## Arguments

- json_data:

  List. Parsed JSON data from a Shinylive `app.json` file. Each element
  should be a list containing:

  - `name`: Character string of the file name

  - `content`: Character string of the file content

  - `type`: Character string indicating the file type

- source_url:

  Character string. The original URL from which the `app.json` was
  downloaded. Used for reference and provenance tracking in the returned
  object.

- output_dir:

  Character string. Directory where application files should be
  extracted. A relative path is created under the current working
  directory; an absolute path is used as-is. Will be created if it
  doesn't exist. Existing files in this directory may be overwritten.

## Value

An object of class `"standalone_shinylive_app"` containing:

- `files`: List of extracted files and their metadata

- `output_dir`: Path to the directory containing extracted files

- `source_url`: Original URL of the application

## Details

The function performs these steps:

1.  Creates the output directory if it doesn't exist

2.  Iterates through each file in the JSON data

3.  Writes each file to the output directory, preserving names

4.  Creates a standalone application object with metadata

File paths are created relative to the output directory. Parent
directories in file paths will be created as needed.

## File Structure

Expected JSON data structure:

    [
      {
        "name": "app.R",
        "content": "library(shiny)\n...",
        "type": "text"
      },
      {
        "name": "data/example.csv",
        "content": "x,y\n1,2\n...",
        "type": "text"
      }
    ]

## See also

- [`create_standalone_shinylive_app()`](https://r-pkg.thecoatlessprofessor.com/peeky/reference/create_standalone_shinylive_app.md)
  for object creation

- [`validate_app_json()`](https://r-pkg.thecoatlessprofessor.com/peeky/reference/validate_app_json.md)
  for JSON data validation

## Examples

``` r
# Example JSON data structure
json_data <- list(
  list(
    name = "app.R",
    content = "library(shiny)\nui <- fluidPage()\n...",
    type = "text"
  ),
  list(
    name = "data.csv",
    content = "col1,col2\n1,2\n3,4",
    type = "text"
  )
)

app <- write_standalone_shinylive_app(
  json_data,
  "https://example.com/app.json",
  file.path(tempdir(), "my_app")
)
```
