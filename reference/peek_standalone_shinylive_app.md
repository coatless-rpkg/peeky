# Download and Extract a Standalone Shinylive Application

Downloads and extracts a standalone Shinylive application from a URL.
The function locates the application's `app.json` file, validates its
structure, and extracts all application files to a local directory.
Works with both R and Python Shinylive applications.

## Usage

``` r
peek_standalone_shinylive_app(url, output_dir)
```

## Arguments

- url:

  Character string. URL pointing to either:

  - A Shinylive app.json file directly

  - A directory containing app.json

  The function will automatically append `"app.json"` to directory URLs.

- output_dir:

  Character string. Directory where the application files should be
  extracted. A relative path is created under the current working
  directory; an absolute path is used as-is. Will be created if it
  doesn't exist. If the directory already exists, files may be
  overwritten.

## Value

An object of class `"standalone_shinylive_app"` containing:

- List of extracted files and their contents

- Source URL of the application

- Output directory location

The object has a custom print method that displays:

- Application type (R or Python)

- Command to run the application

- List of extracted files by type

- File locations

## File Structure

A valid Shinylive application should have an app.json file containing:

- At least one application file (e.g., `app.R` or `app.py`)

- Optional supporting files (e.g., data files, `requirements.txt`)

- File metadata including name, content, and type

## Error Handling

The function will error with informative messages if:

- No `app.json` file is found at the URL

- The `app.json` file has invalid structure

- The `app.json` file cannot be downloaded

- Required application files are missing

## See also

- [`find_shinylive_app_json()`](https://r-pkg.thecoatlessprofessor.com/peeky/reference/find_shinylive_app_json.md)
  for `app.json` validation

- [`write_standalone_shinylive_app()`](https://r-pkg.thecoatlessprofessor.com/peeky/reference/write_standalone_shinylive_app.md)
  for file extraction

- [`peek_shinylive_app()`](https://r-pkg.thecoatlessprofessor.com/peeky/reference/peek_shinylive_app.md)
  for a more general-purpose download function

## Examples

``` r
if (FALSE) { # interactive()
# Download from a direct app.json URL into a temporary directory
app <- peek_standalone_shinylive_app(
  "https://tutorials.thecoatlessprofessor.com/convert-shiny-app-r-shinylive/app.json",
  output_dir = file.path(tempdir(), "standalone_app")
)

# Download from a directory URL (app.json will be appended)
app <- peek_standalone_shinylive_app(
  "https://tutorials.thecoatlessprofessor.com/convert-shiny-app-r-shinylive/",
  output_dir = file.path(tempdir(), "my_local_app")
)

# Print shows how to run the application
print(app)
}
```
