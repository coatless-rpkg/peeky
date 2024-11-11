
<!-- README.md is generated from README.Rmd. Please edit that file -->

# peeky

<!-- badges: start -->

[![R-CMD-check](https://github.com/coatless-rpkg/peeky/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/coatless-rpkg/peeky/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The `peeky` package is a tool for examining and extracting files from
standalone [Shinylive
applications](https://shiny.posit.co/py/docs/shinylive.html) and
[Quarto](https://quarto.org) documents that contain Shinylive components
through the
[`quarto-shinylive`](https://github.com/quarto-ext/shinylive) extension.
This package works for both R and Python Shinylive applications.

## There Are No Secrets in Shinylive

The `peeky` package was developed to demonstrate a fundamental truth
about Shinylive applications stressed by its developers: **“There are no
secrets.”**

Unlike traditional Shiny applications where server-side code remains
private, Shinylive apps run entirely in the web browser, making **all**
associated files accessible to users. This includes the source code,
data, and any other files used by the application. As a result,
Shinylive applications are transparent by design.

This package was developed as part of ongoing discussions in STATS 290
about Shiny application security, transparency, and deployment options.
It serves as a practical demonstration of the differences between
traditional server-side applications and modern browser-based
alternatives.

## Installation

You can install the development version of peeky from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("coatless-rpkg/peeky")
```

## Usage

To use the package, load it into your R session:

``` r
library(peeky)
```

Inside the package, there are three main functions summarized in the
table below:

| Feature | `peek_shinylive_app()` | `peek_standalone_shinylive_app()` | `peek_quarto_shinylive_app()` |
|----|:--:|:--:|:--:|
| Handles standalone apps | ✓ | ✓ | ✗ |
| Handles Quarto docs | ✓ | ✗ | ✓ |
| Auto-detects type | ✓ | ✗ | ✗ |
| Multiple apps per page | ✓ | ✗ | ✓ |
| Custom output path | ✓ | ✓ | ✓ |
| Quarto format output | ✓ | ✗ | ✓ |

### Extracting Shinylive Applications

We suggest using the `peek_shinylive_app()` function as it can handle
both standalone Shinylive applications and Quarto documents with
Shinylive components. For instance, if we take the main Shinylive
extension website, we get:

``` r
peeky::peek_shinylive_app("https://quarto-ext.github.io/shinylive/")
#> 
#> ── Shinylive Applications ──────────────────────────────────────────────────────
#> 
#> ── Python Applications ──
#> 
#> Run in Terminal:
#> shiny run --reload --launch-browser "converted_shiny_app/app_1"
#> shiny run --reload --launch-browser "converted_shiny_app/app_2"
#> shiny run --reload --launch-browser "converted_shiny_app/app_3"
#> shiny run --reload --launch-browser "converted_shiny_app/app_4"
```

This would be equivalent to if we ran the following:

``` r
peeky::peek_quarto_shinylive_app("https://quarto-ext.github.io/shinylive/")
```

By default, the extracted files will be placed in the current working
directory under the `converted_apps` directory. Each application will be
placed in a subdirectory named `app_1`, `app_2`, etc. If we want to
specify a different output directory, we can do so by providing the
`output_path` argument. We can also specify the output format as
`quarto` to extract the files into a single Quarto document.

``` r
# Extract the Shinylive application into a different directory
peeky::peek_quarto_shinylive_app("https://quarto-ext.github.io/shinylive/", output_format = "quarto")
#> 
#> ── Quarto Document with Shinylive Applications ─────────────────────────────────
#> 
#> ── Setup and Preview Steps ──
#> 
#> Step 1: Install the Shinylive extension:
#> quarto add quarto-ext/shinylive
#> 
#> Step 2: Preview the document:
#> quarto preview "converted_shiny_apps.qmd"
#> 
#> ── Contents ──
#> 
#> • R applications: 0
#> • Python applications: 4
```

We can switch to the `peek_shinylive_standalone_app()` function if we
know that the URL is a standalone Shinylive application. For example, if
we take the example application used in the conversion tutorial from [an
app.R to an R Shinylive
app](https://github.com/coatless-tutorials/convert-shiny-app-r-shinylive)
on GitHub, we get:

``` r
peeky::peek_standalone_shinylive_app("https://tutorials.thecoatlessprofessor.com/convert-shiny-app-r-shinylive/")
#> 
#> ── Standalone Shinylive Application ────────────────────────────────────────────
#> Type: R Shiny
#> Run in R:
#> shiny::runApp("converted_shiny_app")
#> 
#> ── Contents ──
#> 
#> .md files:
#> • README.md
#> .R files:
#> • app.R
#> 
#> Total files: 2
#> 
#> Location: 'converted_shiny_app'
```

## License

AGPL (\>= 3)

## Notes

### Evolution from Previous Approaches

This package represents a more refined and comprehensive approach
compared to our [earlier
tutorial](https://github.com/coatless-tutorials/peeking-at-an-r-shinylive-app-source-code)
that focused solely on standalone R Shinylive applications.

### Ethical Considerations

This package is intended for educational purposes and to promote
understanding of web application visibility. Users should:

- Respect intellectual property rights
- Use the tool responsibly
- Understand that the ability to view source code doesn’t imply
  permission to reuse it without proper attribution or licensing
- Consider this knowledge when designing their own applications

## Acknowledgements

We greatly appreciate and are inspired by the work of the Shinylive
team. We also thank the [webR](https://docs.r-wasm.org/webr/latest/) and
[Pyodide](https://pyodide.org/en/stable/) teams for their contributions
to the broader ecosystem of browser-based data science that makes tools
like Shinylive possible.
