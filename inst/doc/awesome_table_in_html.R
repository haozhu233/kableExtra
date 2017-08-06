## ------------------------------------------------------------------------
library(knitr)
library(kableExtra)
dt <- mtcars[1:5, 1:6]

## ------------------------------------------------------------------------
options(knitr.table.format = "html") 
## If you don't define format here, you'll need put `format = "html"` in every kable function.

## ------------------------------------------------------------------------
kable(dt)

## ------------------------------------------------------------------------
kable(dt) %>%
  kable_styling()

## ------------------------------------------------------------------------
kable(dt) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

## ------------------------------------------------------------------------
kable(dt) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

## ------------------------------------------------------------------------
kable(dt) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

## ------------------------------------------------------------------------
kable(dt) %>%
  kable_styling(bootstrap_options = "striped", full_width = F)

## ------------------------------------------------------------------------
kable(dt) %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

## ------------------------------------------------------------------------
kable(dt) %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "float_right")

## ------------------------------------------------------------------------
kable(dt) %>%
  kable_styling(bootstrap_options = "striped", font_size = 7)

## ------------------------------------------------------------------------
kable(dt) %>%
  kable_styling("striped") %>%
  add_header_above(c(" " = 1, "Group 1" = 2, "Group 2" = 2, "Group 3" = 2))

## ------------------------------------------------------------------------
kable(dt) %>%
  kable_styling(c("striped", "bordered")) %>%
  add_header_above(c(" ", "Group 1" = 2, "Group 2" = 2, "Group 3" = 2)) %>%
  add_header_above(c(" ", "Group 4" = 4, "Group 5" = 2)) %>%
  add_header_above(c(" ", "Group 6" = 6))

## ------------------------------------------------------------------------
kable(dt) %>%
  kable_styling("striped") %>%
  add_footnote(c("Footnote 1", "Have a good day."), notation = "alphabet")

## ------------------------------------------------------------------------
kable(dt) %>%
  kable_styling("striped") %>%
  add_footnote(c("Footnote 1", "Have a good day."), notation = "number")

## ------------------------------------------------------------------------
kable(dt) %>%
  kable_styling("striped") %>%
  add_footnote(c("Footnote 1", "Footnote 2", "Footnote 3"), notation = "symbol")

## ------------------------------------------------------------------------
kable(dt, caption = "Demo Table[note]") %>%
  kable_styling("striped") %>%
  add_header_above(c(" ", "Group 1[note]" = 3, "Group 2[note]" = 3)) %>%
  add_footnote(c("This table is from mtcars", 
                 "Group 1 contains mpg, cyl and disp", 
                 "Group 2 contains hp, drat and wt"), 
               notation = "symbol")

## ------------------------------------------------------------------------
kable(mtcars[1:10, 1:6], caption = "Group Rows") %>%
  kable_styling("striped", full_width = F) %>%
  group_rows("Group 1", 4, 7) %>%
  group_rows("Group 2", 8, 10)

## ------------------------------------------------------------------------
kable(dt) %>%
  kable_styling("striped", full_width = F) %>%
  group_rows("Group 1", 3, 5, label_row_css = "background-color: #666; color: #fff;")

## ------------------------------------------------------------------------
kable(dt) %>%
  kable_styling("striped", full_width = F) %>%
  add_indent(c(1, 3, 5))

## ------------------------------------------------------------------------
text_tbl <- data.frame(
  Items = c("Item 1", "Item 2", "Item 3"),
  Features = c(
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin vehicula tempor ex. Morbi malesuada sagittis turpis, at venenatis nisl luctus a. ",
    "In eu urna at magna luctus rhoncus quis in nisl. Fusce in velit varius, posuere risus et, cursus augue. Duis eleifend aliquam ante, a aliquet ex tincidunt in. ", 
    "Vivamus venenatis egestas eros ut tempus. Vivamus id est nisi. Aliquam molestie erat et sollicitudin venenatis. In ac lacus at velit scelerisque mattis. "
  )
)

kable(text_tbl) %>%
  kable_styling(full_width = F) %>%
  column_spec(1, bold = T) %>%
  column_spec(2, width = "30em")

## ------------------------------------------------------------------------
kable(dt) %>%
  kable_styling("striped", full_width = F) %>%
  column_spec(7, bold = T) %>%
  row_spec(5, bold = T)

## ------------------------------------------------------------------------
collapse_rows_dt <- data.frame(C1 = c(rep("a", 10), rep("b", 5)),
                 C2 = c(rep("c", 7), rep("d", 3), rep("c", 2), rep("d", 3)),
                 C3 = 1:15,
                 C4 = sample(c(0,1), 15, replace = TRUE))
kable(collapse_rows_dt, "html", align = "c") %>%
  kable_styling(full_width = F) %>%
  column_spec(1, bold=T) %>%
  collapse_rows(columns = 1:2)

