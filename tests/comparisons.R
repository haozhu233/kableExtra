library(waldo)
library(kableExtra)

comp <- function(..., fn = "kable_styling") {
  old <- get(fn)
  new <- get(paste0(fn, "2"))
  print(sys.call())
  diff <- waldo::compare(capture.output(old(latex, ...)),
                 capture.output(new(latex, ...)))
  if (length(diff) > 0) {
    print(diff)
    stop("have difference")
  }

}

do_comps <- function() {
  table_info <- magic_mirror(latex)
comp()
comp(latex_options = "striped")
comp(latex_options = "hold_position")
comp(latex_options = "HOLD_position")
suppressWarnings(comp(latex_options = "scale_down")) # warnings for longtable
suppressWarnings(comp(latex_options = "scale_up"))   # warnings for longtable
comp(latex_options = "repeat_header")

comp(full_width = TRUE) # expect difference here:  old code loses the [t]
comp(full_width = FALSE)

comp(position = "left")
comp(position = "center")
if (table_info$tabular == "longtable")
  try(suppressWarnings(comp(position = "right")))
else
  suppressWarnings(comp(position = "right"))
suppressWarnings(comp(position = "float_left")) # warnings from both for longtable
if (table_info$tabular == "longtable")
  try(suppressWarnings(comp(position = "float_right"))) # warnings from both
else
  suppressWarnings(comp(position = "float_right"))
comp(font_size = 12)

comp(row_label_position = "l")
#comp(row_label_position = "c") # expect difference:  old code gives "l" not "c"
#comp(row_label_position = "r") # expect difference:  old code gives "l" not "r"

comp(repeat_header_text = "header test text")

comp(repeat_header_method = "append")
comp(repeat_header_method = "replace")

comp(repeat_header_continued = TRUE)
comp(repeat_header_continued = FALSE)
comp(repeat_header_continued = "continued test text")

comp(stripe_color = "red", latex_options = "striped")

comp(stripe_index = 3, latex_options = "striped")

try(
comp(latex_table_env = "tabularx") # expect differences:  old code loses [t]
)

comp(table.envir = "table*", position = "center")

comp(wraptable_width = "5pt")
}

do_comps2 <- function() {
  comp(2, fn = "row_spec")
  comp(2, bold = TRUE, fn = "row_spec")
  comp(2, italic = TRUE, fn = "row_spec")
  comp(2, monospace = TRUE, fn = "row_spec")
  comp(2, underline = TRUE, fn = "row_spec")
  comp(2, strikeout = TRUE, fn = "row_spec")
  comp(2, color = "red", fn = "row_spec")
  comp(2, background = "red", fn = "row_spec")
  comp(2, align = "l", fn = "row_spec")
  comp(2, align = "c", fn = "row_spec")
  comp(2, align = "r", fn = "row_spec")
  comp(2, font_size = 12, fn = "row_spec")
  comp(2, angle = 45, fn = "row_spec")
  comp(2, hline_after = TRUE, fn = "row_spec")
  comp(2, extra_latex_after = "This is extra!", fn = "row_spec")
}

latex <- kbl(head(mtcars), format="latex")

do_comps2()
do_comps()

latex <- kbl(head(mtcars), format="latex", longtable = TRUE, caption = "The caption")

do_comps2()
do_comps()

latex <- kbl(head(mtcars), format="latex", longtable = TRUE)

do_comps2()
do_comps()

latex <- kbl(head(mtcars), format="latex")

do_comps2()
do_comps()

latex <- kbl(head(mtcars), format="latex", booktabs = TRUE, longtable = TRUE)

do_comps2()
do_comps()

latex <- kbl(head(mtcars), format="latex", booktabs = TRUE, longtable = TRUE,
             caption = "The caption")

do_comps2()
do_comps()
