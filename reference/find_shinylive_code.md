# Find Shinylive Code Blocks in Quarto HTML

Parses HTML content to extract and process Shinylive code blocks for
both R and Python applications. This function identifies code blocks
with class `'shinylive-r'` or `'shinylive-python'` and processes their
content into structured application data.

## Usage

``` r
find_shinylive_code(html)
```

## Arguments

- html:

  Character string containing HTML content. The HTML should contain code
  blocks with class `'shinylive-r'` or `'shinylive-python'` to be
  processed.

## Value

A list of parsed Shinylive applications. Each list element contains:

- `engine`: Character string indicating the application type (`"r"` or
  `"python"`)

- `options`: List of parsed YAML-style options from the code block

- `files`: List of file definitions, where each file contains:

  - `name`: Character string of the file name

  - `content`: Character string of the file content

  - `type`: Character string indicating the file type

## Details

The function performs the following steps:

1.  Parses the HTML content using `rvest`

2.  Extracts code blocks with classes `'shinylive-r'` or
    `'shinylive-python'`

3.  For each code block:

    - Determines the engine type from the 'data-engine' attribute

    - Extracts the code text content

    - Parses the code block structure using
      [`parse_code_block()`](https://r-pkg.thecoatlessprofessor.com/peeky/reference/parse_code_block.md)

Code blocks should follow the Shinylive format with optional YAML-style
options (prefixed with `'#|'`) and file markers (prefixed with
`'## file:'`).

## See also

parse_code_block
