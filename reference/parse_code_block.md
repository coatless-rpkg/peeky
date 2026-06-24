# Parse a Single Shinylive Code Block

Parses the content of a Shinylive code block, extracting YAML-style
options, file definitions, and content. Handles both single-file and
multi-file applications for R and Python.

## Usage

``` r
parse_code_block(code_text, engine)
```

## Arguments

- code_text:

  Character string. The raw text content of a Shinylive code block,
  which may contain YAML-style options, file markers, and file content.

- engine:

  Character string. The type of Shinylive application, either `"r"` or
  `"python"`. Determines default file extension when no explicit file is
  specified.

## Value

A list with three components:

- `engine`: Character string indicating the application type (`"r"` or
  `"python"`)

- `options`: List of parsed YAML-style options from block headers

- `files`: Named list of file definitions, where each file contains:

  - `name`: Character string of the file name

  - `content`: Character string of the file content

  - `type`: Character string indicating the file type (defaults to
    \`"text"“)

## Code Block Structure

The code block can contain several types of lines:

- **YAML-style options:** Lines starting with \`'#\|'“

- **File markers:** Lines starting with `'## file:'`

- **Type markers:** Lines starting with `'## type:'`

- **Content:** All other non-empty lines

For single-file applications with no explicit file marker, the content
is automatically placed in:

- `"app.R"` for R applications

- `"app.py"` for Python applications

## See also

- [`parse_yaml_options()`](https://r-pkg.thecoatlessprofessor.com/peeky/reference/parse_yaml_options.md)
  for YAML-style option parsing

- [`find_shinylive_code()`](https://r-pkg.thecoatlessprofessor.com/peeky/reference/find_shinylive_code.md)
  for extracting code blocks from HTML
