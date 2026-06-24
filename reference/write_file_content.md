# Write File Content in a Shinylive App to Disk

Writes file content extracted from Shinylive applications to disk,
handling both text and binary content appropriately. Creates any
necessary parent directories and ensures proper encoding of content. For
binary files, automatically decodes the base64-encoded content before
writing.

## Usage

``` r
write_file_content(content, file_path, type = "text")
```

## Arguments

- content:

  Character string containing the file content. For binary files, this
  should be base64-encoded content. For text files, this should be the
  raw text content.

- file_path:

  Character string specifying the path where the file should be written.
  Parent directories will be created if they don't exist.

- type:

  Character string specifying the file type, either "text" (default) or
  "binary". Binary files are assumed to be base64 encoded, as this is
  the standard format for binary content in Shinylive applications.

## Value

Invisible NULL, called for its side effect of writing a file to disk.

## Details

The function handles two types of content:

- Text files (`type = "text"`):

  - Content is converted to UTF-8 encoding using
    [`enc2utf8()`](https://rdrr.io/r/base/Encoding.html)

  - Written using
    [`writeLines()`](https://rdrr.io/r/base/writeLines.html) with
    `useBytes = TRUE`

- Binary files (`type = "binary"`):

  - Content is decoded from base64 using
    [`jsonlite::base64_dec()`](https://jeroen.r-universe.dev/jsonlite/reference/base64.html)

  - Written as raw binary data using
    [`writeBin()`](https://rdrr.io/r/base/readBin.html)

Parent directories in the file path are automatically created if they
don't exist using
[`fs::dir_create()`](https://fs.r-lib.org/reference/create.html) with
`recurse = TRUE`.

## Examples

``` r
# Write a text file into a temporary directory
write_file_content(
  content = "library(shiny)\n\nui <- fluidPage()",
  file_path = file.path(tempdir(), "app", "app.R"),
  type = "text"
)

# Write a base64-encoded image
b64img <- paste0(
  "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAA",
  "DUlEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg=="
)
write_file_content(b64img, file.path(tempdir(), "test.png"), type = "binary")
```
