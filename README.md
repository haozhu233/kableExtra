# kableExtra <img src="https://haozhu233.github.io/kableExtra/kableExtra.svg" align="right" alt="logo" width="120" height = "139" style = "border: none; float: right;">
<!-- badges: start -->
[![CRAN_version](https://www.r-pkg.org/badges/version/kableExtra)](https://cran.r-project.org/package=kableExtra)
[![CRAN_download](https://cranlogs.r-pkg.org/badges/kableExtra)](https://cran.r-project.org/package=kableExtra)
[![CRAN_download_total](https://cranlogs.r-pkg.org/badges/grand-total/kableExtra)](https://cran.r-project.org/package=kableExtra)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1035917.svg)](https://doi.org/10.5281/zenodo.1035917)
[![R-CMD-check](https://github.com/haozhu233/kableExtra/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/haozhu233/kableExtra/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

***

When we are talking about table generators in R, [knitr](https://yihui.org/knitr/)'s `kable()` function is usually a popular choice because of its ultimate simplicity. Unlike those powerful table rendering engines such as [`xtable`](https://CRAN.R-project.org/package=xtable), the philosophy behind [`knitr::kable()`](https://rdrr.io/cran/knitr/man/kable.html) is to make it easy for programmers to use. Just as it claimed in its function description,

> This is a very simple table generator. It is simple by design. It is not intended to replace any other R packages for making tables. - Yihui

However, the ultimate simplicity of `kable()` also brought troubles to some of us, especially for new R users, who may not have a lot of experience on generating tables in R. It is not rare to see people including experienced users asking questions like how to center/left-align a table on Stack Overflow. Also, for me personally, I found myself repeatedly parsing CSS into `kable()` for some very simple features like striped lines. For LaTeX, it's even worse since I'm almost Stack Overflow dependent for LaTeX... That's why this package `kableExtra` was created.

I hope with `kableExtra`, you can

- Use default base `kable()` (Or a good alternative for markdown tables is `pander::pander()`) for all simple tables
- Use `kable()` with `kableExtra` to generate 90 % of complex/advanced/self-customized/beautiful tables in either HTML or LaTeX
- Only have to mess with raw HTML/LaTeX in the last 10% cases where `kableExtra` cannot solve the problem

***

This package can load required LaTeX package automatically in vanilla R Markdown. For customized R Markdown templates, it is recommended to load related LaTeX packages manually.

***

## Features
### Pipable syntax
`kableExtra` is NOT a table generating package. It is a package that can **"add features"** to a `kable()` output using a syntax that every useR loves - the [pipes `%>%`](https://r4ds.had.co.nz/pipes.html). We see similar approaches to deal with plots in packages like `ggvis` and `plotly`. There is no reason why we cannot use it with tables.

### Unified functions for both HTML and PDF
Most functionalities in `kableExtra` can work in both HTML and PDF. In fact, as long as you specifies format in `kable()` (which can be set globally through option `knitr.table.format`), functions in this package will pick the right way to manipulate the table be themselves. As a result, if users want to left align the table, `kable(...) %>% kable_styling(position = "left")` will work in both HTML and PDF. Recently, we also introduced a new `kbl()` function acting as an alternative to `kable` but provides better documentation and format detection. 

## Install
```r
install.packages("kableExtra")

# For dev version
devtools::install_github("haozhu233/kableExtra")
```

## Basic Usage
```r
library(kableExtra)
dt <- mtcars[1:5, 1:4]

# HTML table
kbl(dt, caption = "Demo Table") %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F) %>%
  add_header_above(c(" ", "Group 1" = 2, "Group 2[note]" = 2)) %>%
  footnote(c("table footnote"))

# LaTeX Table
kbl(dt, booktabs = T, caption = "Demo Table") %>%
  kable_styling(latex_options = c("striped", "hold_position"),
                full_width = F) %>%
  add_header_above(c(" ", "Group 1" = 2, "Group 2[note]" = 2)) %>%
  footnote(c("table footnote"))

```
### Results
<img src="http://i.imgur.com/0e2b4P3.png" height="200" />
<img src="http://i.imgur.com/q46hzORm.png" height="200" />

## More Information
For more information, please check the package vignette.

- [Create Awesome HTML Table with `knitr::kable()` and kableExtra](http://haozhu233.github.io/kableExtra/awesome_table_in_html.html) ([中文](http://haozhu233.github.io/kableExtra/awesome_table_in_html_cn.html))
- [Create Awesome LaTeX Table with `knitr::kable()` and kableExtra](http://haozhu233.github.io/kableExtra/awesome_table_in_pdf.pdf)


## Acknowledgement
I would like to thank colleagues at Hebrew SeniorLife [Marcus Institute for Aging Research](https://www.marcusinstituteforaging.org/) and the [Boston Pepper Center](https://pepper.bwh.harvard.edu/) for their input. I also would like to appreciate the mentorship from Tom Travison ([@tgt75](https://twitter.com/tgt75)) and all the efforts from the open source community, which help this package keep getting better.
