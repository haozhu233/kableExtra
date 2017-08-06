## ------------------------------------------------------------------------
library(knitr)
library(kableExtra)
dt <- mtcars[1:5, 1:6]

## ------------------------------------------------------------------------
options(knitr.table.format = "latex") 
## If you don't define format here, you'll need put `format = "latex"` 
## in every kable function.

## ------------------------------------------------------------------------
kable(dt)

## ------------------------------------------------------------------------
kable(dt, booktabs = T)

## ------------------------------------------------------------------------
kable(dt, booktabs = T) %>%
  kable_styling(latex_options = "striped")

## ------------------------------------------------------------------------
kable(dt, caption = "Demo table", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "hold_position"))

## ------------------------------------------------------------------------
kable(cbind(dt, dt, dt), booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down"))

## ------------------------------------------------------------------------
kable(cbind(dt), booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down"))

## ------------------------------------------------------------------------
long_dt <- rbind(mtcars, mtcars) 

kable(long_dt, longtable = T, booktabs = T, caption = "Longtable") %>%
  add_header_above(c(" ", "Group 1" = 5, "Group 2" = 6)) %>%
  kable_styling(latex_options = c("repeat_header"))

## ------------------------------------------------------------------------
kable(dt, booktabs = T) %>%
  kable_styling(full_width = T)

## ------------------------------------------------------------------------
kable(dt, booktabs = T) %>%
  kable_styling(position = "center")

## ------------------------------------------------------------------------
kable(dt, booktabs = T) %>%
  kable_styling(position = "float_right")

## ------------------------------------------------------------------------
kable(dt, booktabs = T) %>%
  kable_styling(font_size = 7)

## ------------------------------------------------------------------------
text_tbl <- data.frame(
  Items = c("Item 1", "Item 2", "Item 3"),
  Features = c(
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin vehicula tempor ex. Morbi malesuada sagittis turpis, at venenatis nisl luctus a. ",
    "In eu urna at magna luctus rhoncus quis in nisl. Fusce in velit varius, posuere risus et, cursus augue. Duis eleifend aliquam ante, a aliquet ex tincidunt in. ", 
    "Vivamus venenatis egestas eros ut tempus. Vivamus id est nisi. Aliquam molestie erat et sollicitudin venenatis. In ac lacus at velit scelerisque mattis. "
  )
)

kable(text_tbl, booktabs = T) %>%
  kable_styling(full_width = F) %>%
  column_spec(1, bold = T) %>%
  column_spec(2, width = "30em")

## ------------------------------------------------------------------------
kable(dt, booktabs = T) %>%
  kable_styling("striped", full_width = F) %>%
  column_spec(7, bold = T) %>%
  row_spec(5, bold = T)

## ------------------------------------------------------------------------
kable(dt, booktabs = T) %>%
  kable_styling() %>%
  add_header_above(c(" " = 1, "Group 1" = 2, "Group 2" = 2, "Group 3" = 2))

## ------------------------------------------------------------------------
kable(dt, booktabs = T) %>%
  kable_styling(latex_options = "striped") %>%
  add_header_above(c(" ", "Group 1" = 2, "Group 2" = 2, "Group 3" = 2)) %>%
  add_header_above(c(" ", "Group 4" = 4, "Group 5" = 2)) %>%
  add_header_above(c(" ", "Group 6" = 6), bold = T, italic = T)

## ------------------------------------------------------------------------
kable(mtcars[1:10, 1:6], caption = "Group Rows", booktabs = T) %>%
  kable_styling() %>%
  group_rows("Group 1", 4, 7) %>%
  group_rows("Group 2", 8, 10)

## ------------------------------------------------------------------------
kable(dt, booktabs = T) %>%
  group_rows("Group 1", 4, 5, latex_gap_space = "2em")

## ------------------------------------------------------------------------
kable(dt, booktabs = T) %>%
  add_indent(c(1, 3, 5))

## ------------------------------------------------------------------------
collapse_rows_dt <- data.frame(C1 = c(rep("a", 10), rep("b", 5)),
                 C2 = c(rep("c", 7), rep("d", 3), rep("c", 2), rep("d", 3)),
                 C3 = 1:15,
                 C4 = sample(c(0,1), 15, replace = TRUE))
kable(collapse_rows_dt, "latex", booktabs = T, align = "c") %>%
  column_spec(1, bold=T) %>%
  collapse_rows(columns = 1:2)

## ------------------------------------------------------------------------
kable(collapse_rows_dt, "latex", align = "c") %>%
  column_spec(1, bold = T, width = "5em") %>%
  collapse_rows(1:2)

## ------------------------------------------------------------------------
kable(dt, booktabs = T) %>%
  kable_styling() %>%
  add_footnote(c("Footnote 1", "Have a good day."), notation = "alphabet")

## ------------------------------------------------------------------------
kable(dt, booktabs = T) %>%
  kable_styling() %>%
  add_footnote(c("Footnote 1", "Have a good day."), notation = "number")

## ------------------------------------------------------------------------
kable(dt, booktabs = T) %>%
  kable_styling() %>%
  add_footnote(c("Footnote 1", "Footnote 2", "Footnote 3"), notation = "symbol")

## ------------------------------------------------------------------------
kable(dt, caption = "Demo Table[note]", booktabs = T) %>%
  kable_styling(latex_options = "hold_position") %>%
  add_header_above(c(" ", "Group 1[note]" = 3, "Group 2[note]" = 3)) %>%
  add_footnote(c("This table is from mtcars", 
                 "Group 1 contains mpg, cyl and disp", 
                 "Group 2 contains hp, drat and wt"), 
               notation = "symbol")

## ------------------------------------------------------------------------
kable(dt, caption = "Demo Table (Landscape)[note]", booktabs = T) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  add_header_above(c(" ", "Group 1[note]" = 3, "Group 2[note]" = 3)) %>%
  add_footnote(c("This table is from mtcars", 
                 "Group 1 contains mpg, cyl and disp", 
                 "Group 2 contains hp, drat and wt"), 
               notation = "symbol") %>%
  group_rows("Group 1", 4, 5) %>%
  landscape()

