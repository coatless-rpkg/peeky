# Find and Validate Shinylive app.json

Attempts to locate and validate a Shinylive app.json file from a given
base URL. The function tries multiple possible paths and validates both
the HTTP response and JSON structure.

## Usage

``` r
find_shinylive_app_json(base_url)
```

## Arguments

- base_url:

  Character string. The base URL to search for app.json.

## Value

A list with three components:

- `valid` Logical indicating if a valid app.json was found

- `url` Character string of the successful URL, or NULL if not found

- `data` List containing the parsed JSON data if valid, NULL otherwise

## Details

The function performs the following steps:

1.  Generates three possible paths to check:

    - The base URL as provided

    - base URL + \`"/app.json"“

    - Parent directory + `"/app.json"`

2.  For each path:

    - Attempts an HTTP GET request

    - Verifies the content type is JSON

    - Parses and validates the JSON structure

    - Returns immediately if valid app.json is found
