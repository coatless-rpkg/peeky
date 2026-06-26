# peeky

The `peeky` package helps you extract, examine, and run the source code
from Shiny applications that have been converted to run in the browser
using [Shinylive](https://shiny.posit.co/py/get-started/shinylive.html).
It works with both standalone applications and
[Quarto](https://quarto.org) documents containing Shinylive components
through the [quarto-shinylive](https://github.com/quarto-ext/shinylive)
extension, supporting both R and Python Shiny applications.

## About Shinylive

[Shinylive](https://shiny.posit.co/py/get-started/shinylive.html)
converts existing Shiny applications to run entirely in the web browser
using [WebAssembly](https://webassembly.org/) versions of R
([webR](https://docs.r-wasm.org/webr/latest/)) and Python
([Pyodide](https://pyodide.org/en/stable/)), eliminating the need for a
computational server. This means all application files are accessible to
users by design as they are downloaded to the user’s browser when the
application is loaded.

The Shinylive ecosystem consists of four main components:

- Shinylive Web Assets:
  [posit-dev/shinylive](https://github.com/posit-dev/shinylive)
- R Shiny App Converter Package:
  [posit-dev/r-shinylive](https://github.com/posit-dev/r-shinylive)
- Python Shiny App Convert Package:
  [posit-dev/py-shinylive](https://github.com/posit-dev/py-shinylive)
- Shinylive Quarto Extension:
  [quarto-ext/shinylive](https://github.com/quarto-ext/shinylive)

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

Inside the package, there are three main functions:

| Function | Description |
|----|----|
| [`peek_shinylive_app()`](https://r-pkg.thecoatlessprofessor.com/peeky/reference/peek_shinylive_app.md) | Universal function that handles both standalone apps and Quarto docs |
| [`peek_standalone_shinylive_app()`](https://r-pkg.thecoatlessprofessor.com/peeky/reference/peek_standalone_shinylive_app.md) | Specifically for standalone Shinylive applications |
| [`peek_quarto_shinylive_app()`](https://r-pkg.thecoatlessprofessor.com/peeky/reference/peek_quarto_shinylive_app.md) | Specifically for Quarto documents with Shinylive components |

### Extracting Shinylive Applications

We suggest using the
[`peek_shinylive_app()`](https://r-pkg.thecoatlessprofessor.com/peeky/reference/peek_shinylive_app.md)
function as it can handle both standalone Shinylive applications and
Quarto documents with Shinylive components. For instance, if we take the
main Shinylive extension website, we get:

``` r

# Choose where the files are written (here, a temporary directory)
out_dir <- file.path(tempdir(), "shinylive-apps")
peeky::peek_shinylive_app("https://quarto-ext.github.io/shinylive/", output_dir = out_dir)
#> 
#> ── Shinylive Applications ──────────────────────────────────────────────────────
#> 
#> ── Shiny for Python Applications ──
#> 
#> Run in Terminal:
#> shiny run --reload --launch-browser "/tmp/RtmpXXXXXX/shinylive-apps/app_1/app.py"
#> shiny run --reload --launch-browser "/tmp/RtmpXXXXXX/shinylive-apps/app_2/app.py"
#> shiny run --reload --launch-browser "/tmp/RtmpXXXXXX/shinylive-apps/app_3/app.py"
#> shiny run --reload --launch-browser "/tmp/RtmpXXXXXX/shinylive-apps/app_4/app.py"
```

This would be equivalent to if we ran the following:

``` r

peeky::peek_quarto_shinylive_app(
  "https://quarto-ext.github.io/shinylive/",
  output_path = out_dir
)
```

The output location is a required argument, so the package never writes
to your working directory unless you ask it to: a relative path such as
`"my-apps"` is created under the current working directory, while an
absolute path is used as-is. Each application is placed in a
subdirectory named `app_1`, `app_2`, etc. We can also set the output
format to `quarto` to extract the files into a single Quarto document.

``` r

# Extract the applications into a single Quarto document
peeky::peek_quarto_shinylive_app(
  "https://quarto-ext.github.io/shinylive/",
  output_format = "quarto",
  output_path = file.path(tempdir(), "shinylive-apps.qmd")
)
#> 
#> ── Quarto Document with Shinylive Applications ─────────────────────────────────
#> 
#> ── Setup and Preview Steps ──
#> 
#> Step 1: Change to the document directory:
#> cd "/tmp/RtmpXXXXXX"
#> 
#> Step 2: Install the Shinylive extension:
#> quarto add quarto-ext/shinylive
#> 
#> Step 3: Preview the document:
#> quarto preview "shinylive-apps.qmd"
#> 
#> ── Contents ──
#> 
#> • R applications: 0
#> • Python applications: 4
```

We can switch to the
[`peek_standalone_shinylive_app()`](https://r-pkg.thecoatlessprofessor.com/peeky/reference/peek_standalone_shinylive_app.md)
function if we know that the URL is a standalone Shinylive application.
For example, if we take the example application used in the conversion
tutorial from [an app.R to an R Shinylive
app](https://github.com/coatless-tutorials/convert-shiny-app-r-shinylive)
on GitHub, we get:

``` r

peeky::peek_standalone_shinylive_app(
  "https://tutorials.thecoatlessprofessor.com/convert-shiny-app-r-shinylive/",
  output_dir = file.path(tempdir(), "standalone-app")
)
#> 
#> ── Standalone Shinylive Application ────────────────────────────────────────────
#> Type: R Shiny
#> Run in R:
#> shiny::runApp("/tmp/RtmpXXXXXX/standalone-app")
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
#> Location: '/tmp/RtmpXXXXXX/standalone-app'
```

## License

AGPL (\>= 3)

## Evolution

This package represents a more refined and comprehensive approach
compared to our [earlier
tutorial](https://github.com/coatless-tutorials/peeking-at-an-r-shinylive-app-source-code)
that focused solely on standalone R Shinylive applications.

## Ethical Considerations

This package is for educational purposes. Users should:

- Use responsibly
- Respect intellectual property rights
- Understand that viewable code doesn’t imply permission to reuse
- Consider this when designing their own applications

## Acknowledgements

Thanks to the Shinylive, [webR](https://docs.r-wasm.org/webr/latest/)
and [Pyodide](https://pyodide.org/en/stable/) teams for enabling
browser-based data science.
