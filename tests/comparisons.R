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

  comp(fn = "landscape")
  comp(margin = "1cm", fn = "landscape")
  comp(fn = "footnote")
  comp(general = "General comment", fn = "footnote")
  comp(general = "General comment", general_title = "General title", fn = "footnote")
  comp(general = "General comment", general_title = "General title", title_format = "bold", fn = "footnote")
  comp(number = c("Footnote 1", "Footnote 2"), fn = "footnote")
  comp(number = c("Footnote 1", "Footnote 2"), number_title = "Number title", fn = "footnote")
  comp(alphabet = c("Footnote a", "Footnote b"), fn = "footnote")
  comp(alphabet = c("Footnote a", "Footnote b"),
       alphabet_title = "Alpha title", fn = "footnote")
  comp(symbol = c("Symbol 1", "Symbol 2"), fn = "footnote")
  comp(symbol = c("Symbol 1", "Symbol 2"), symbol_manual = c('*', '**', '***'), fn = "footnote") # symbol_manual handled differently, so avoid backslash
  comp(symbol = c("Symbol 1", "Symbol 2"),
       symbol_title = "Symbol title", fn = "footnote")
  comp(general = "General comment", alphabet = c("Footnote a", "Footnote b"), footnote_order = c("alphabet", "general"), fn = "footnote")
  comp(general = "General comment General comment General comment General comment General comment General comment",
       footnote_as_chunk = TRUE, fn = "footnote")
  comp(general = "this is math: $1 + 1$", escape = TRUE, fn = "footnote")
  comp(general = "this is math: $1 + 1$", escape = FALSE, fn = "footnote")
  comp(general = "General comment General comment General comment General comment General comment General comment",
       threeparttable = TRUE, fn = "footnote")
  comp(general = "General comment", fixed_small_size = TRUE,
       fn = "footnote")
  try(comp(general = "General comment", show_every_page = TRUE,
       fn = "footnote")) # old code is wrong for longtable

  comp(column = 2, fn = "column_spec")
  comp(column = 2, width = "2cm", fn = "column_spec")
  comp(column = 2, bold = TRUE, fn = "column_spec")
  comp(column = 2, monospace = TRUE, fn = "column_spec")
  comp(column = 2, color = "red", fn = "column_spec")
  comp(column = 2, background = "red", fn = "column_spec")
  comp(column = 2, border_left = TRUE, fn = "column_spec")
  comp(column = 2, border_right = TRUE, fn = "column_spec")
  comp(column = 2, latex_column_spec = "p{1cm}", fn = "column_spec")
  comp(column = 2, latex_valign = "m", width = "2cm", fn = "column_spec")
  comp(column = 2, link = "https://www.r-project.org", fn = "column_spec")
  comp(column = 2, link = "https://www.r-project.org", new_tab = FALSE, fn = "column_spec")
  comp(column = 2, image = "https://www.r-project.org/Rlogo.png", fn = "column_spec")

  comp(fn = "collapse_rows")
  comp(fn = "collapse_rows", valign = "bottom")
  comp(fn = "collapse_rows", columns = 3)
  try(comp(fn = "collapse_rows", latex_hline = "major")) # old gives extra newlines
  comp(fn = "collapse_rows", headers_to_remove = 3)

  comp(positions = 2:3, fn = "add_indent")
  comp(positions = 2:3, all_cols = TRUE, fn = "add_indent")
  comp(positions = 2:3, target_cols = 3:4, fn = "add_indent")

  comp(group_label = "The label", start_row = 2,
       end_row = 5, fn = "pack_rows")
  comp(group_label = "The label", start_row = 2,
       end_row = 5, latex_gap_space = "5pt", fn = "pack_rows")
  comp(group_label = "The label", start_row = 2,
       end_row = 5, latex_align = "r", fn = "pack_rows")
  comp(group_label = "The label", start_row = 2,
       end_row = 5, bold = TRUE, fn = "pack_rows")
  comp(group_label = "The label", start_row = 2,
       end_row = 5, hline_before = TRUE, fn = "pack_rows")
  comp(group_label = "The label", start_row = 2,
       end_row = 5, hline_after = TRUE, fn = "pack_rows")
  comp(group_label = "The label", start_row = 2,
       end_row = 5, color = "red", fn = "pack_rows")

  comp(header = c(" ", "Group 1" = 5, "Group 2" = 6), fn = "add_header_above")
  comp(header = c(" ", "Group 1" = 5, "Group 2" = 6), bold = TRUE, fn = "add_header_above")
  comp(header = c(" ", "Group 1" = 5, "Group 2" = 6), italic = TRUE, fn = "add_header_above")
  comp(header = c(" ", "Group 1" = 5, "Group 2" = 6), monospace = TRUE, fn = "add_header_above")
  comp(header = c(" ", "Group 1" = 5, "Group 2" = 6), underline = TRUE, fn = "add_header_above")
  comp(header = c(" ", "Group 1" = 5, "Group 2" = 6), strikeout = TRUE, fn = "add_header_above")
  comp(header = c(" ", "Group 1" = 5, "Group 2" = 6), align = "l", fn = "add_header_above")
  comp(header = c(" ", "Group 1" = 5, "Group 2" = 6), align = "r", fn = "add_header_above")
  comp(header = c(" ", "Group 1" = 5, "Group 2" = 6), align = "p{2cm}", fn = "add_header_above")

  comp(label = "The footnote", fn = "add_footnote")
  comp(label = "The footnote", notation = "number", fn = "add_footnote")
  comp(label = "The footnote", notation = "symbol", fn = "add_footnote")
  comp(label = "The footnote", notation = "none", fn = "add_footnote")
  comp(label = "The footnote", threeparttable = TRUE, fn = "add_footnote")
  comp(label = "The footnote costs $2", fn = "add_footnote")
  comp(label = "The footnote has math:  $1+1$", escape = FALSE, fn = "add_footnote")

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
do_comps()

latex <- kbl(head(mtcars), format="latex", longtable = TRUE, caption = "The caption")
do_comps()

latex <- kbl(head(mtcars), format="latex", longtable = TRUE)
do_comps()

latex <- kbl(head(mtcars), format="latex")
do_comps()

latex <- kbl(head(mtcars), format="latex", booktabs = TRUE, longtable = TRUE)
do_comps()

latex <- kbl(head(mtcars), format="latex", booktabs = TRUE, longtable = TRUE,
             caption = "The caption")
do_comps()
