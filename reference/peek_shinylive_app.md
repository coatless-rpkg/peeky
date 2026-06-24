# Download and Extract Shinylive Applications from URLs

Downloads and extracts Shinylive applications from various URL sources.
The function can handle both standalone Shinylive applications and
Quarto documents containing embedded Shinylive applications. It
automatically detects the application type and extracts the necessary
files.

## Usage

``` r
peek_shinylive_app(url, output_dir)
```

## Arguments

- url:

  Character string. URL pointing to one of:

  - A standalone Shinylive application (containing `app.json`)

  - A directory containing `app.json`

  - A Quarto document with embedded Shinylive applications

- output_dir:

  Character string. Directory where the application files should be
  extracted. A relative path is created under the current working
  directory; an absolute path is used as-is. The directory will be
  created if it doesn't exist.

## Value

An object containing the extracted application information:

- For standalone apps: Object of class `"standalone_shinylive_app"`

- For Quarto documents: Object of class `"quarto_shinylive_apps"`

Both object types implement custom print methods that display:

- Application type (R or Python)

- Commands to run the application

- List of extracted files

- Output directory location

## Details

The function follows these steps:

1.  Downloads and analyzes the content at the provided URL

2.  Determines if the content is a Quarto document or standalone
    application

3.  For Quarto documents:

    - Extracts all embedded Shinylive applications

    - Creates separate directories for each application

4.  For standalone applications:

    - Locates and validates the `app.json` file

    - Extracts all application files to the specified directory

## URL Resolution

The function attempts several strategies to find app.json:

- Direct use of the provided URL

- Appending `"app.json"` to the URL

- Checking the parent directory

## Error Handling

The function will error with informative messages if:

- The URL cannot be accessed

- No valid Shinylive application is found

- The `app.json` structure is invalid

## See also

- [`peek_quarto_shinylive_app()`](https://r-pkg.thecoatlessprofessor.com/peeky/reference/peek_quarto_shinylive_app.md)
  for handling Quarto documents specifically

- [`peek_standalone_shinylive_app()`](https://r-pkg.thecoatlessprofessor.com/peeky/reference/peek_standalone_shinylive_app.md)
  for handling standalone applications

## Examples

``` r
if (FALSE) { # interactive()
# Write the extracted files to the session's temporary directory
url <- "https://tutorials.thecoatlessprofessor.com/convert-shiny-app-r-shinylive/"
app <- peek_shinylive_app(url, output_dir = file.path(tempdir(), "my_extracted_app"))

# Download from a Quarto document
apps <- peek_shinylive_app(
  "https://quarto-ext.github.io/shinylive/",
  output_dir = file.path(tempdir(), "my_extracted_apps")
)
}
```
