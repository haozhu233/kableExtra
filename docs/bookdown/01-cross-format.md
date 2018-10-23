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

## Prepare Your Tables for All Formats
In most cases, functions in `kable` and `kableExtra` use the same API to accomplish the same styling task in HTML and LaTeX. However, you also need some format specific settings so your tables will look good in both formats. Some common items here include the `booktabs` and `longtable` settings in `kable` and the `bootstrap_options` and `latex_options` in `kable_styling`. 

Here is an example for a table that will work in HTML, LaTeX & EPUB.


```r
library(kableExtra)
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.5.1
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
options(kableExtra.html.bsTable = T)
iris[1:10, ] %>%
  mutate_if(is.numeric, function(x) {
    cell_spec(x, bold = T, 
              color = spec_color(x, end = 0.9),
              font_size = spec_font_size(x))
  }) %>%
  mutate(Species = cell_spec(
    Species, color = "white", bold = T,
    background = spec_color(1:10, end = 0.9, 
                            option = "A", direction = -1)
  )) %>%
  kable(escape = F, align = "c", booktabs = T) %>%
  kable_styling(c("striped", "condensed"), 
                latex_options = "striped", 
                full_width = F)
```

<table class="table table-striped table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> Sepal.Length </th>
   <th style="text-align:center;"> Sepal.Width </th>
   <th style="text-align:center;"> Petal.Length </th>
   <th style="text-align:center;"> Petal.Width </th>
   <th style="text-align:center;"> Species </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: rgba(40, 174, 128, 1);font-size: 14px;">5.1</span> </td>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: rgba(31, 154, 138, 1);font-size: 13px;">3.5</span> </td>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: rgba(62, 75, 138, 1);font-size: 10px;">1.4</span> </td>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: rgba(53, 96, 141, 1);font-size: 11px;">0.2</span> </td>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: white;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(254, 206, 145, 1);">setosa</span> </td>
  </tr>
  <tr>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: rgba(37, 131, 142, 1);font-size: 12px;">4.9</span> </td>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: rgba(72, 34, 116, 1);font-size: 9px;">3</span> </td>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: rgba(62, 75, 138, 1);font-size: 10px;">1.4</span> </td>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: rgba(53, 96, 141, 1);font-size: 11px;">0.2</span> </td>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: white;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(254, 160, 109, 1);">setosa</span> </td>
  </tr>
  <tr>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: rgba(57, 87, 140, 1);font-size: 10px;">4.7</span> </td>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: rgba(56, 88, 140, 1);font-size: 10px;">3.2</span> </td>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: rgba(68, 1, 84, 1);font-size: 8px;">1.3</span> </td>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: rgba(53, 96, 141, 1);font-size: 11px;">0.2</span> </td>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: white;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(246, 110, 92, 1);">setosa</span> </td>
  </tr>
  <tr>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: rgba(67, 62, 133, 1);font-size: 10px;">4.6</span> </td>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: rgba(67, 62, 133, 1);font-size: 10px;">3.1</span> </td>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: rgba(37, 131, 142, 1);font-size: 12px;">1.5</span> </td>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: rgba(53, 96, 141, 1);font-size: 11px;">0.2</span> </td>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: white;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(222, 73, 104, 1);">setosa</span> </td>
  </tr>
  <tr>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: rgba(31, 154, 138, 1);font-size: 13px;">5</span> </td>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: rgba(41, 175, 127, 1);font-size: 14px;">3.6</span> </td>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: rgba(62, 75, 138, 1);font-size: 10px;">1.4</span> </td>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: rgba(53, 96, 141, 1);font-size: 11px;">0.2</span> </td>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: white;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(183, 55, 121, 1);">setosa</span> </td>
  </tr>
  <tr>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: rgba(187, 223, 39, 1);font-size: 16px;">5.4</span> </td>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: rgba(187, 223, 39, 1);font-size: 16px;">3.9</span> </td>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: rgba(187, 223, 39, 1);font-size: 16px;">1.7</span> </td>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: rgba(187, 223, 39, 1);font-size: 16px;">0.4</span> </td>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: white;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(140, 41, 129, 1);">setosa</span> </td>
  </tr>
  <tr>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: rgba(67, 62, 133, 1);font-size: 10px;">4.6</span> </td>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: rgba(37, 131, 142, 1);font-size: 12px;">3.4</span> </td>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: rgba(62, 75, 138, 1);font-size: 10px;">1.4</span> </td>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: rgba(34, 168, 132, 1);font-size: 13px;">0.3</span> </td>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: white;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(100, 26, 128, 1);">setosa</span> </td>
  </tr>
  <tr>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: rgba(31, 154, 138, 1);font-size: 13px;">5</span> </td>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: rgba(37, 131, 142, 1);font-size: 12px;">3.4</span> </td>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: rgba(37, 131, 142, 1);font-size: 12px;">1.5</span> </td>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: rgba(53, 96, 141, 1);font-size: 11px;">0.2</span> </td>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: white;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(60, 15, 112, 1);">setosa</span> </td>
  </tr>
  <tr>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: rgba(68, 1, 84, 1);font-size: 8px;">4.4</span> </td>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: rgba(68, 1, 84, 1);font-size: 8px;">2.9</span> </td>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: rgba(62, 75, 138, 1);font-size: 10px;">1.4</span> </td>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: rgba(53, 96, 141, 1);font-size: 11px;">0.2</span> </td>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: white;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(20, 14, 54, 1);">setosa</span> </td>
  </tr>
  <tr>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: rgba(37, 131, 142, 1);font-size: 12px;">4.9</span> </td>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: rgba(67, 62, 133, 1);font-size: 10px;">3.1</span> </td>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: rgba(37, 131, 142, 1);font-size: 12px;">1.5</span> </td>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: rgba(68, 1, 84, 1);font-size: 8px;">0.1</span> </td>
   <td style="text-align:center;"> <span style=" font-weight: bold;    color: white;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(0, 0, 4, 1);">setosa</span> </td>
  </tr>
</tbody>
</table>
