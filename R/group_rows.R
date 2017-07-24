#' Put a few rows of a table into one category
#'
#' @description Group a few rows in a table together under a label.
#'
#' @param kable_input Output of `knitr::kable()` with `format` specified
#' @param group_label A character string for the name of the group
#' @param start_row A numeric value that tells the function in which row the
#' group starts. Note that the counting excludes header rows and other group
#' labeling rows
#' @param end_row A numeric value that tells the function in which row the group
#' ends.
#' @param label_row_css A character string for any customized css used for the
#' labeling row. By default, the labeling row will have a solid black line
#' underneath. Only useful for HTML documents.
#' @param latex_gap_space A character value telling LaTeX how large the gap
#' between the previous row and the group labeling row. Only useful for LaTeX
#' documents.
#'
#' @examples x <- knitr::kable(head(mtcars), "html")
#' # Put Row 2 to Row 5 into a Group and label it as "Group A"
#' group_rows(x, "Group A", 2, 5)
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
  out <- as_kable_xml(kable_xml)
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
  } else {
    rowtext <- paste0("\\\\hline\n", rowtext)
    new_rowtext <- paste0(
      "\\\\hline\n\\\\multicolumn{", table_info$ncol, "}{l}{\\\\textbf{",
      group_label, "}}\\\\\\\\\n", rowtext
    )
  }
  out <- sub(rowtext, new_rowtext, out)
  out <- gsub("\\\\addlinespace\n", "", out)
  table_info$group_rows_used <- TRUE
  attr(out, "kable_meta") <- table_info
  out <- add_indent(out, seq(start_row, end_row))
  return(out)
}
