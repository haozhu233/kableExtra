<!-- README.md is generated from README.Rmd. Please edit that file -->
This package is still in an "as-is" state. You can save a lot of finger-typing time by using it but you still need to understand what is really going on behind the hood, which is still far behind my goal. Also, since the default output format of `kable` is `markdown`, which doesn't support high-level table customization, it may not work well in many cases. I would recommend you to set the `format` option in each `kable` function or to define `options(knitr.table.format = 'html')` or `latex` somewhere in your document.

Introduction to kableExtra
==========================

When we are talking about table generators in `R`, `knitr::kable` wins the favor of a lot of people by its ultimate simplicity. Unlike those powerful table rendering engine such as `xtable`, `tables` or even `gridExtra`, the philosophy behind `kable` is to make it easy for programmers to use. Just as it claimed in its function description, &gt; This is a very simple table generator. It is simple by design. It is not intended to replace any other R packages for making tables. - Yihui

However, we also see a lot of people getting frustrated online for the lack of functionality of `kable`. It is kind of unfair to `kable` as it is supposed to be "simple" by design. However, it is also understandable as people cry because they love to use this function instead of those complicated alternatives.

In `kableExtra`, we are not intended to build another table generator engine as there have been a lot (actually too many in my personal opinion) in `R`. This package is an attempt to extend `knitr::kable`'s functionality without destroying the beauty of its simplicity by using the pipe syntax from `magrittr`. We will follow the literal programming practice and try to make the progress of building a table in `R` go together with the flow of logic in your mind. We will also borrow the idea of *"grammar of graphics"* from `ggplot2` and `ggvis` and implement it in the progress of table generating tasks.

Vocabulary & Grammar
--------------------

Here is a list of features that we would love to see in this package. I bolded these that have been done.

-   **add\_footnote()**
-   add\_indent()
-   col\_markup()
-   row\_markup()
-   add\_stripe()
-   add\_hover()
-   add\_textcolor()
-   add\_bgcolor()
-   **magic\_mirror()**
-   reverse\_kable()

The syntax of this package is just like what you are doing in `dplyr` or `ggvis`. Here is an example of adding footnote to a `kable` object.

``` r
library(knitr)
library(kableExtra)

cars %>%
  head() %>%
  rename("speed[note]" = speed) %>% 
  kable(caption = "Head of cars [note]") %>%
  add_footnote(
    label = c("Footnote in caption", 
      "Footnote in table"),
    notation = "number" # Or "alphabet"/"symbol"
  )
```
