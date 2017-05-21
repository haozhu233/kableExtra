#' Put a few rows of a table into one category
#'
#' @export
group_rows <- function(kable_input, group_label, start_row, end_row) {
  kable_format <- attr(kable_input, "format")
  if (!kable_format %in% c("html", "latex")) {
    message("Currently generic markdown table using pandoc is not supported.")
    return(kable_input)
  }
  if (kable_format == "html") {
    return(group_rows_html(kable_input, group_label, start_row, end_row))
  }
  if (kable_format == "latex") {
    return(group_rows_latex(kable_input, group_label, start_row, end_row))
  }
}

group_rows_html <- function(kable_input, group_label, start_row, end_row) {
  if (!is.numeric(c(start_row, end_row))) {
    stop("Start_row and end_row must be numeric position of rows (excluding",
         "header rows and other group-title rows). ")
  }
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
    '<tr groupLength="', length(group_seq), '"><td colspan="',
    kable_ncol, '"><strong>', group_label, "</strong></td></tr>"
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
