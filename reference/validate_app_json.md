# Validate Shinylive `app.json` Structure

Validates that a parsed `app.json` structure meets the requirements for
a Shinylive application. Checks for proper list structure, required
fields, and non-empty content. Provides detailed error messages when
validation fails.

## Usage

``` r
validate_app_json(json_data)
```

## Arguments

- json_data:

  List. Parsed JSON data from an app.json file. Should be a list where
  each element represents a file and contains:

  - `name`: Character string of the file name

  - `content`: Character string of the file content

  - `type`: Character string indicating the file type

## Value

Logical TRUE if validation passes. If validation fails, throws an error
with detailed information about the validation failure using
cli_abort().

## Validation Rules

The function checks these requirements:

1.  `json_data` must be a list

2.  `json_data` must contain at least one element

3.  Each element must be a list (representing a file)

4.  Each file list must contain all required fields:

    - `name`

    - `content`

    - `type`

## Error Messages

The function provides detailed error messages for these cases:

- Not a list: "Expected a list or array of files"

- Empty list: "File list is empty"

- Invalid file entry: "Each entry must be a file object"

- Missing fields: Lists specific missing required fields

## Expected JSON Structure

The expected JSON structure is an array of objects, where each object
represents a file in the application.

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

- [`find_shinylive_app_json()`](https://r-pkg.thecoatlessprofessor.com/peeky/reference/find_shinylive_app_json.md)
  which uses this validation
