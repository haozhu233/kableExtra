# Cross-format Tables in Bookdown

> Note that if kableExtra 0.9.0 doesn't support the EPUB format. If you need to output tables in `.epub`, please upgrade to the dev version or version 1.0 on CRAN once it's released.

## Use the "K-M" approach instead of "M-K"
Please read this chapter about the "K-M"/"M-K" approaches in `bookdown`:
https://bookdown.org/yihui/bookdown/new-session.html

To generate cross-format tables with `kableExtra` in a multi-format bookdown project, you will have to use the "M-K" approach by setting `new_session: true` in `_bookdown.yml`. Somehow the "M-K" approach, which merges chapters to a big Rmd and then renders, shares the global environment across formats. For now, I'm not sure if this is a bug or intended behavior. It might be fixable in the future but please don't count on that. By setting `new_session: true`, we force R to use a new session for every chapter for different formats. In this way, tables are generated differently in different formats. 

Note that the "M-K" approach is slower than the "K-M" approach. At the same time, packages and data are not shared accross chapter.

```
# Example _bookdown.yml
book_filename: "bookdown_example"
delete_merged_file: true
new_session: true
language:
  ui:
    chapter_name: "Chapter "
```

## Prepare Your Tables for Both Formats
In most cases, functions in `kable` and `kableExtra` use the same API to accomplish the same styling task in HTML and LaTeX. However, you also need some format specific settings so your tables will look good in both formats. Some common items here include the `booktabs` and `longtable` settings in `kable` and the `bootstrap_options` and `latex_options` in `kable_styling`. 

Here is an example for a table that will work in both HTML and LaTeX.


```r
library(kableExtra)
options(kableExtra.html.bsTable = T)
mtcars[1:5, 1:5] %>%
  kable(booktabs = T) %>% 
  kable_styling(
    latex_options = c("striped"),
    full_width = F
  ) %>%
  column_spec(1, bold = T) %>%
  add_header_above(c(" ", "Group A" = 2, "Group B" = 3))
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
<tr>
<th style="border-bottom:hidden" colspan="1"></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Group A</div></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="3"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Group B</div></th>
</tr>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> mpg </th>
   <th style="text-align:right;"> cyl </th>
   <th style="text-align:right;"> disp </th>
   <th style="text-align:right;"> hp </th>
   <th style="text-align:right;"> drat </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;font-weight: bold;"> Mazda RX4 </td>
   <td style="text-align:right;"> 21.0 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 160 </td>
   <td style="text-align:right;"> 110 </td>
   <td style="text-align:right;"> 3.90 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> Mazda RX4 Wag </td>
   <td style="text-align:right;"> 21.0 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 160 </td>
   <td style="text-align:right;"> 110 </td>
   <td style="text-align:right;"> 3.90 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> Datsun 710 </td>
   <td style="text-align:right;"> 22.8 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 108 </td>
   <td style="text-align:right;"> 93 </td>
   <td style="text-align:right;"> 3.85 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> Hornet 4 Drive </td>
   <td style="text-align:right;"> 21.4 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 258 </td>
   <td style="text-align:right;"> 110 </td>
   <td style="text-align:right;"> 3.08 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> Hornet Sportabout </td>
   <td style="text-align:right;"> 18.7 </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 360 </td>
   <td style="text-align:right;"> 175 </td>
   <td style="text-align:right;"> 3.15 </td>
  </tr>
</tbody>
</table>
