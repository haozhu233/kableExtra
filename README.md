# kableExtra
[![CRAN_version](http://www.r-pkg.org/badges/version/kableExtra)](https://cran.r-project.org/package=kableExtra)
[![CRAN_download](http://cranlogs.r-pkg.org/badges/kableExtra)](https://cran.r-project.org/package=kableExtra)

***

**If you find using `kableExtra` will "unload" LaTeX packages you defined in the yaml header, you need to install the latest dev version of `rmarkdown` from github. Please see [issue 5](https://github.com/haozhu233/kableExtra/issues/5) & [issue 11](https://github.com/haozhu233/kableExtra/issues/11) for details.**

***

When we are talking about table generators in R, knitr::kable wins lots of flavor by its ultimate simplicity. Unlike those powerful table rendering engines such as `xtable`, the philosophy behind kable is to make it easy for programmers to use. Just as it claimed in its function description, 

> This is a very simple table generator. It is simple by design. It is not intended to replace any other R packages for making tables. - Yihui

However, the ultimate simplicity of `kable()` also brought troubles to some of us, especially for new R users, who may not have a lot of experience on generating tables in R. It is not rare to see people including experienced users asking questions like how to center/left-align a table on Stack Overflow. Also, for me personally, I found myself repeatedly parsing CSS into `kable()` for some very simple features like striped lines. For LaTeX, it's even worse since I'm almost Stack Overflow dependent for LaTeX... That's why this package `kableExtra` was created. 

I hope with `kableExtra`, you can

- Use default base `kable()` (Or a good alternative for markdown tables is `pander::pander()`) for all simple tables
- Use `kable()` with `kableExtra` to generate 90 % of complex/advanced/self-customized/beautiful tables in either HTML or LaTeX
- Only have to mess with raw HTML/LaTeX in the last 10% cases where `kableExtra` cannot solve the problem

***

This package can load required LaTeX package automatically in vanilla rmarkdown. For customized rmarkdown templates, it is recommended to load related LaTeX packages manually.

***

## Features
### Pipable syntax
`kableExtra` is NOT a table generating package. It is a package that can **"add features"** to a `kable()` output using a syntax that every useR loves - the pipes `%>%`. We see similar approaches to deal with plots in packages like `ggvis` and `plotly`. There is no reason why we cannot use it with tables. 

### Unified functions for both HTML and PDF
Most functionalities in `kableExtra` can work in both HTML and PDF. In fact, as long as you specifies format in `kable()` (which can be set globally through option `knitr.table.format`), functions in this package will pick the right way to manipulate the table be themselves. As a result, if users want to left align the table, `kable(...) %>% kable_styling(position = "left")` will work in both HTML and PDF. 

## Install
```r
install.packages("kableExtra")

# For dev version
devtools::install_github("haozhu233/kableExtra")
```

## Basic Usage
```r
library(knitr)
library(kableExtra)
dt <- mtcars[1:5, 1:4]

# HTML table
kable(dt, format = "html", caption = "Demo Table") %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F) %>%
  add_header_above(c(" ", "Group 1" = 2, "Group 2[note]" = 2)) %>%
  add_footnote(c("table footnote"))

# LaTeX Table
kable(dt, format = "latex", booktabs = T, caption = "Demo Table") %>%
  kable_styling(latex_options = c("striped", "hold_position"), 
                full_width = F) %>%
  add_header_above(c(" ", "Group 1" = 2, "Group 2[note]" = 2)) %>%
  add_footnote(c("table footnote"))
  
```
### Results
<img src="http://i.imgur.com/0e2b4P3.png" height="200" />
<img src="http://i.imgur.com/q46hzORm.png" height="200" />

## More Information
For more information, please check the package vignette.

- [Create Awesome HTML Table with knitr::kable and kableExtra](http://haozhu233.github.io/kableExtra/awesome_table_in_html.html)
- [Create Awesome LaTeX Table with knitr::kable and kableExtra](http://haozhu233.github.io/kableExtra/awesome_table_in_pdf.pdf)

## Limitations/Known bug
- `add_header_above` and `add_footnote` should be able to work in any conditions but if you are using `kable_styling` in customed templates it can get a little tricky. 
- In HTML, `kable_styling` assumes you to have bootstrap 3 style sheet loaded to have all features functioning. 
- In LaTeX, it is known that striped lines is not working with tufte handout since right now I cannot insert a latex package to its LaTeX header. 
- If you are using the CRAN version, when you run rmarkdown from console using RScript, you need to include `library(methods)` at the begining due to a `rvest` issue. By default, `library(methods)` is loaded in R GUI and RStudio. I eliminated the step requires `rvest` in current github version since it doesn't provide a very useful feature. See [this SO post for details](http://stackoverflow.com/questions/35351443/rvest-html-nodes-error-cannot-coerce-type-environment-to-vector-of-type-l). Thanks twitter user @Gimperion for reporting. 

## To-do
- [x] A function to insert a gap row (or group title row in the middle of a table)
- [x] A function to easily add title column indent
- ~~[ ] Some ways to change text/background color of cells~~
- [ ] Find a way to let align table caption.
- [x] Add `column_spec`
- [x] Add `row_spec`
- [ ] Add a clickable js module for shiny app integration
