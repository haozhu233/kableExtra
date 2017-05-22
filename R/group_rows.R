#' Put a few rows of a table into one category
#'
#' @export
group_rows <- function(kable_input, group_label, start_row, end_row,
                       label_row_css = "border-bottom: 1px solid;",
                       latex_gap_space = "0.5em") {
  if (!is.numeric(c(start_row, end_row))) {
    stop("Start_row and end_row must be numeric position of rows (excluding",
         "header rows and other group-title rows). ")
  }
  kable_format <- attr(kable_input, "format")
  if (!kable_format %in% c("html", "latex")) {
    message("Currently generic markdown table using pandoc is not supported.")
    return(kable_input)
  }
  if (kable_format == "html") {
    return(group_rows_html(kable_input, group_label, start_row, end_row,
                           label_row_css))
  }
  if (kable_format == "latex") {
    return(group_rows_latex(kable_input, group_label, start_row, end_row,
                            latex_gap_space))
  }
}

group_rows_html <- function(kable_input, group_label, start_row, end_row,
                            label_row_css) {
  kable_attrs <- attributes(kable_input)
  kable_xml <- read_xml(as.character(kable_input), options = "COMPACT")
  kable_tbody <- xml_tpart(kable_xml, "tbody")

  group_header_rows <- attr(kable_input, "group_header_rows")
  group_seq <- seq(start_row, end_row)
  if (!is.null(group_header_rows)) {
    group_seq <- positions_corrector(group_seq, group_header_rows,
                                     length(xml_children(kable_tbody)))
  }

  # Insert a group header row
  starting_node <- xml_child(kable_tbody, group_seq[1])
  kable_ncol <- length(xml_children(starting_node))
  group_header_row_text <- paste0(
    '<tr groupLength="', length(group_seq), '"><td colspan="', kable_ncol,
    '" style="', label_row_css, '"><strong>', group_label,
    "</strong></td></tr>"
  )
  group_header_row <- read_xml(group_header_row_text, options = "COMPACT")
  xml_add_sibling(starting_node, group_header_row, .where = "before")

  # add indentations to items
  out <- structure(as.character(kable_xml), format = "html",
                   class = "knitr_kable")
  attributes(out) <- kable_attrs
  attr(out, "group_header_rows") <- c(attr(out, "group_header_rows"), group_seq[1])
  out <- add_indent(out, positions = seq(start_row, end_row))
  return(out)
}

group_rows_latex <- function(kable_input, group_label, start_row, end_row,
                             gap_space) {
  table_info <- magic_mirror(kable_input)
  out <- kable_input

  # Add group label
  rowtext <- table_info$contents[start_row + 1]
  if (table_info$booktabs) {
    new_rowtext <- paste0(
      "\\\\addlinespace[", gap_space, "]\n",
      "\\\\multicolumn{", table_info$ncol, "}{l}{\\\\textbf{", group_label,
      "}}\\\\\\\\\n",
      rowtext
    )
    # last_row <- paste0(table_info$contents[end_row + 1], "\\\\\\\\")
    # out <- sub(
    #   last_row,
    #   paste0(last_row, "\n\\\\addlinespace[", gap_space, "]"),
    #   out
    # )
  } else {
    rowtext <- paste0("\\\\hline\n", rowtext)
    new_rowtext <- paste0(
      "\\\\hline\n\\\\multicolumn{", table_info$ncol, "}{l}{\\\\textbf{",
      group_label, "}}\\\\\\\\\n", rowtext
    )
  }
  out <- sub(rowtext, new_rowtext, out)
  attr(out, "original_kable_meta") <- table_info
  out <- add_indent(out, seq(start_row, end_row))
  return(out)
}
