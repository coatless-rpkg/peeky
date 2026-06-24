# Parse YAML-style Options from Shinylive Code Blocks

Parses YAML-style configuration options from Shinylive code block
headers. These options appear as lines prefixed with `'#|'` and follow a
simplified YAML-like syntax for key-value pairs.

## Usage

``` r
parse_yaml_options(yaml_lines)
```

## Arguments

- yaml_lines:

  Character vector. Each element should be a line containing a
  YAML-style option in the format `'#| key: value'`. The `'#|'` prefix
  will be stripped during processing.

## Value

A named list of parsed options where:

- Array values (e.g., `'[1, 2, 3]'`) are converted to character vectors

- Boolean values ('true'/'false') are converted to logical values

- Numeric values are converted to numeric type

- Other values remain as character strings

## Details

The function handles several value types:

- **Arrays:** Values in the format `'[item1, item2, ...]'`

- **Booleans:** Values 'true' or 'false'

- **Numbers:** Integer values

- **Strings:** All other values

Lines that don't contain a colon (`':'`) are ignored.

## See also

parse_code_block
