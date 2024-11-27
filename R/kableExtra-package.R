#' kableExtra
#'
#' @description When we are talking about table generators in R,
#' [knitr](https://yihui.org/knitr/)'s `kable()` function wins lots of flavor
#' by its ultimate simplicity. Unlike those powerful table rendering engines
#' such as [`xtable`](https://CRAN.R-project.org/package=xtable), the philosophy
#' behind [`knitr::kable()`](https://rdrr.io/cran/knitr/man/kable.html) is to
#' make it easy for programmers to use. Just as it claimed in its
#' function description, "this is a very simple table generator. It is simple
#' by design. It is not intended to replace any other R packages for making
#' tables. - Yihui".
#'
#' However, the ultimate simplicity of `kable()` also brought troubles to some
#' of us, especially for new R users, who may not have a lot of experience on
#' generating tables in R. It is not rare to see people including experienced
#' users asking questions like how to center/left-align a table on Stack
#' Overflow. Also, for me personally, I found myself repeatedly parsing CSS
#' into `kable()` for some very simple features like striped lines. For LaTeX,
#' it's even worse since I'm almost Stack Overflow dependent for LaTeX...
#' That's why this package `kableExtra` was created.
#'
#' I hope with `kableExtra`, you can
#' - Use default base `kable()` (Or a good alternative for markdown tables is
#' `pander::pander()`) for all simple tables
#' - Use `kable()` with `kableExtra` to generate 90 % of complex/advanced
#' tables in either HTML or LaTeX
#' - Only have to mess with raw HTML/LaTeX in the last 10% cases where
#' `kableExtra` cannot solve the problem
#'
#' For a full package documentation, please visit the
#' [package documentation site](https://haozhu233.github.io/kableExtra/)
#' for more information
#'
#' @section Features:
#' **Pipable syntax:** `kableExtra` is NOT a table generating package. It is a
#' package that can "add features" to a `kable` output using a syntax
#' that every useR loves - the [pipe](https://r4ds.had.co.nz/pipes.html).
#' We see similar approaches to deal with plots in packages like `ggvis` and
#' `plotly`. There is no reason why we cannot use it with tables.
#'
#' **Unified functions for both HTML and PDF:** Most functionalities in
#' `kableExtra` can work in both HTML and PDF. In fact, as long as you
#' specifies format in `kable` (which can be set globally through option
#' `knitr.table.format`), functions in this package will pick the right way
#' to manipulate the table be themselves. As a result, if users want to left
#' align the table, `kable_styling(kable(...), position = "left")` will work
#' in both HTML and PDF.
#'
#' @note If you found a feature on the documentation site that is not available
#' in the version of `kableExtra` you are using, try to install the pre-release
#' version from GitHub. You can do so by running
#' `devtools::install_github("haozhu233/kableExtra")`.
#'
#' Also, note that This package can load required LaTeX package automatically in
#' vanilla R Markdown. For customized R Markdown templates, it is recommended to
#' load related LaTeX packages manually.
#'
#' @importFrom stringr fixed str_count str_split str_match str_detect str_match_all
#' str_extract str_replace_all str_trim str_extract_all str_sub str_replace
#' @importFrom xml2 read_xml xml_attr xml_has_attr xml_attr<- read_html
#' xml_child xml_children xml_name xml_add_sibling xml_add_child xml_text xml_length
#' xml_remove write_xml xml_text<- xml_length
#' @importFrom knitr knit_meta_add include_graphics knit_print asis_output kable
#' @importFrom rmarkdown latex_dependency html_dependency_bootstrap
#' html_dependency_jquery pandoc_self_contained_html
#' @importFrom magrittr %>%
#' @importFrom utils read.csv head capture.output
#' @importFrom scales rescale
#' @importFrom viridisLite viridis
#' @importFrom stats ave density median na.omit setNames
#' @importFrom grDevices col2rgb svg png
#' @importFrom rstudioapi isAvailable viewer
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom digest digest
#' @importFrom graphics par text hist boxplot
#' @importFrom svglite svglite
#' @import htmltools
#' @name kableExtra-package
#' @aliases kableExtra
#' @keywords package
"_PACKAGE"

#' @export
magrittr::`%>%`

#' @export
knitr::kable
