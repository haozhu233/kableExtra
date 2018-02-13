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
#' @param index A named vector providing the index for robust row-grouping tasks.
#' Basically, you can use it in the same way as `add_header_above()`.
#' @param label_row_css A character string for any customized css used for the
#' labeling row. By default, the labeling row will have a solid black line
#' underneath. Only useful for HTML documents.
#' @param latex_gap_space A character value telling LaTeX how large the gap
#' between the previous row and the group labeling row. Only useful for LaTeX
#' documents.
#' @param escape A T/F value showing whether special characters should be
#' escaped.
#' @param align Adjust justification of group_label in latex only. Value should be "c" for
#' centered on row, "r" for right justification, or "l" for left justification. Default
#' Value is "l"  If using html, the alignment can be set by using the label_row_css
#' parameter.
#' @param colnum A numeric that determines how many columns the text should span.
#' The default setting will have the text span the entire length.
#'
#' @examples x <- knitr::kable(head(mtcars), "html")
#' # Put Row 2 to Row 5 into a Group and label it as "Group A"
#' group_rows(x, "Group A", 2, 5)
#'
#' @export
group_rows <- function(kable_input, group_label = NULL,
                       start_row = NULL, end_row = NULL,
                       index = NULL,
                       label_row_css = "border-bottom: 1px solid;",
                       latex_gap_space = "0.3em",
                       escape = TRUE, align = "l", colnum = NULL) {

  kable_format <- attr(kable_input, "format")
  if (!kable_format %in% c("html", "latex")) {
    warning("Please specify format in kable. kableExtra can customize either ",
            "HTML or LaTeX outputs. See https://haozhu233.github.io/kableExtra/ ",
            "for details.")
    return(kable_input)
  }
  if (is.null(index)) {
    if (kable_format == "html") {
      if(!missing(align)) warning("Align parameter is not used in HTML Mode,
                                    use label_row_css instead.")
      return(group_rows_html(kable_input, group_label, start_row, end_row,
                             label_row_css, escape, colnum))
    }
    if (kable_format == "latex") {
      return(group_rows_latex(kable_input, group_label, start_row, end_row,
                              latex_gap_space, escape, align, colnum))
    }
  } else {
    index <- group_row_index_translator(index)
    out <- kable_input
    if (kable_format == "html") {
      for (i in 1:nrow(index)) {
        if(!missing(align)) warning("Align parameter is not used in HTML Mode,
                                    use label_row_css instead.")
        out <- group_rows_html(out, index$header[i],
                               index$start[i], index$end[i],
                               label_row_css, escape, colnum)
      }
    }
    if (kable_format == "latex") {
      for (i in 1:nrow(index)) {
        out <- group_rows_latex(out, index$header[i],
                               index$start[i], index$end[i],
                               latex_gap_space, escape, align, colnum)
      }
    }
    return(out)
  }
}

group_row_index_translator <- function(index) {
  index <- standardize_header_input(index)
  index$start <- cumsum(c(1, index$colspan))[1:length(index$colspan)]
  index$end <- cumsum(index$colspan)
  index$header <- trimws(index$header)
  index <- index[index$header != "", ]
  return(index)
}

group_rows_html <- function(kable_input, group_label, start_row, end_row,
                            label_row_css, escape, colnum) {
  kable_attrs <- attributes(kable_input)
  kable_xml <- read_kable_as_xml(kable_input)
  kable_tbody <- xml_tpart(kable_xml, "tbody")

  if (escape) {
    group_label <- escape_html(group_label)
  }

  group_header_rows <- attr(kable_input, "group_header_rows")
  group_seq <- seq(start_row, end_row)
  if (!is.null(group_header_rows)) {
    group_seq <- positions_corrector(group_seq, group_header_rows,
                                     length(xml_children(kable_tbody)))
  }

  # Insert a group header row
  starting_node <- xml_child(kable_tbody, group_seq[1])
  kable_ncol <- ifelse(is.null(colnum),
                       length(xml_children(starting_node)),
                       colnum)
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
  out <- add_indent_html(out, positions = seq(start_row, end_row))
  return(out)
}

group_rows_latex <- function(kable_input, group_label, start_row, end_row,
                             gap_space, escape, align, colnum) {
  table_info <- magic_mirror(kable_input)
  out <- enc2utf8(as.character(kable_input))

  if (table_info$duplicated_rows) {
    dup_fx_out <- fix_duplicated_rows_latex(out, table_info)
    out <- dup_fx_out[[1]]
    table_info <- dup_fx_out[[2]]
  }

  if (escape) {
    group_label <- escape_latex(group_label)
    group_label <- gsub("\\\\", "\\\\\\\\", group_label)
  }

  # Add group label
  rowtext <- table_info$contents[start_row + 1]
  if (table_info$booktabs) {
    new_rowtext <- paste0(
      "\\\\addlinespace[", gap_space, "]\n",
      "\\\\multicolumn{", ifelse(is.null(colnum),
                                 table_info$ncol,
                                 colnum),
      "}{", align, "}{\\\\textbf{", group_label,
      "}}\\\\\\\\\n",
      rowtext
    )
  } else {
    rowtext <- paste0("\\\\hline\n", rowtext)
    new_rowtext <- paste0(
      "\\\\hline\n\\\\multicolumn{", table_info$ncol, "}{", align,"}{\\\\textbf{",
      group_label, "}}\\\\\\\\\n", rowtext
    )
  }
  out <- sub(rowtext, new_rowtext, out)
  out <- gsub("\\\\addlinespace\n", "", out)
  out <- structure(out, format = "latex", class = "knitr_kable")
  table_info$group_rows_used <- TRUE
  attr(out, "kable_meta") <- table_info
  out <- add_indent_latex(out, seq(start_row, end_row))
  return(out)
}
