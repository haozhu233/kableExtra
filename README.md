# kableExtra
When we are talking about table generators in R, knitr::kable wins the favor of a lot of people by its ultimate simplicity. Unlike those powerful table rendering engine such as xtable, tables or even gridExtra, the philosophy behind kable is to make it easy for programmers to use. Just as it claimed in its function description, 

> This is a very simple table generator. It is simple by design. It is not intended to replace any other R packages for making tables. - Yihui

However, the ultimate simplicity of `kable()` brought troubles to some people, especially some new R users who may not have got exposed to other table making packages in R. It is not rare to see people including experienced user asking questions like how to center/left-align a table on Stack Overflow or twitter. These are the reasons why this package `kableExtra` was created. 

I hope with `kableExtra`, you can

- Use default base `kable()` for all simple tables
- Use `kable()` with `kableExtra` to generate 90 % of complex/advanced/self-customized/beautiful tables in either HTML or LaTeX
- Only have to mess with raw HTML/LaTeX in the last 10% cases where `kableExtra` cannot solve the problem

## Features
### Pipable syntax
`kableExtra` is NOT a table generating package. It is a package that can **"add features"** to a `kable()` output using a syntax that every useR loves - the pipes `%>%`. We see similar approaches to deal with plots in packages like `ggvis` and `plotly`. There is no reason why we cannot use it with tables. 

### Unified functions for both HTML and PDF
Most functionalities in `kableExtra` can work in both HTML and PDF. In fact, as long as you specifies format in `kable()` (which can be set globally through option `knitr.table.format`), functions in this package will pick the right way to manipulate the table be themselves. As a result, if users want to left align the table, `kable(...) %>% kable_styling(position = "left")` will work in both HTML and PDF. 

## Install
```r
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
<img src="http://i.imgur.com/kHFBF3Hm.png" style="height: 200px;"/>
<img src="http://i.imgur.com/q46hzORm.png" style="height: 200px;"/>
