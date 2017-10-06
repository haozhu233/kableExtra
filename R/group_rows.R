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
                       latex_gap_space = "0.3em", latex_indent = "1em",
                       escape = TRUE) {

  kable_format <- attr(kable_input, "format")
  if (!kable_format %in% c("html", "latex")) {
    message("Currently generic markdown table using pandoc is not supported.")
    return(kable_input)
  }
  if (is.null(index)) {
    if (kable_format == "html") {
      return(group_rows_html(kable_input, group_label, start_row, end_row,
                             label_row_css, escape))
    }
    if (kable_format == "latex") {
      return(group_rows_latex(kable_input, group_label, start_row, end_row,
                              latex_gap_space, latex_indent, escape))
    }
  } else {
    index <- group_row_index_translator(index)
    out <- kable_input
    if (kable_format == "html") {
      for (i in 1:nrow(index)) {
        out <- group_rows_html(out, index$header[i],
                               index$start[i], index$end[i],
                               label_row_css, escape)
      }
    }
    if (kable_format == "latex") {
      for (i in 1:nrow(index)) {
        out <- group_rows_latex(out, index$header[i],
                               index$start[i], index$end[i],
                               latex_gap_space, latex_indent, escape)
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
                            label_row_css, escape) {
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
  out <- add_indent_html(out, positions = seq(start_row, end_row))
  return(out)
}

group_rows_latex <- function(kable_input, group_label, start_row, end_row,
                             gap_space, indent, escape) {
  table_info <- magic_mirror(kable_input)
  # out <- enc2utf8(as.character(kable_input)) # I am not sure whether encoding
  # is neccessary before or also after (where it would be simpler) the
  # operations here, so I leave it commented out instead of changing it.
  contents <- table_info$contents
  contents <- add_indent_latex(contents, seq(start_row, end_row), indent)

  if (escape) {
    group_label <- escape_latex(group_label)
    group_label <- gsub("\\\\", "\\\\\\\\", group_label)
  }

  # Add group label
  rowtext <- contents[start_row + 1]
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
  contents[start_row+1] <- new_rowtext
  table_info$contents <- contents
  out <- reassemble_kable(magic_mirror = table_info)
  out <- structure(out, format = "latex", class = "knitr_kable")
  table_info$group_rows_used <- TRUE
  attr(out, "kable_meta") <- table_info
  return(out)
}

reassemble_kable <- function(magic_mirror){

  contents <- magic_mirror$contents

  contents <- gsub("\\\\", "\\", contents, fixed = TRUE)
  contents <- gsub("\\$", "$", contents, fixed = TRUE)
  contents <- gsub("\\{", "{", contents, fixed = TRUE)
  contents <- gsub("\\}", "}", contents, fixed = TRUE)
  contents <- gsub("\\(", "(", contents, fixed = TRUE)
  contents <- gsub("\\)", ")", contents, fixed = TRUE)
  # probably others as well?

  magic_mirror$contents <- contents

  begin <- paste0("\n\\begin{", magic_mirror$tabular, "}[", magic_mirror$valign3, "]{", magic_mirror$align, "}\n")
  caption <- paste0("\\caption[", magic_mirror$caption.short, "]{", magic_mirror$caption, "}\\\\\n")
  header <- paste0("\\toprule\n", magic_mirror$contents[1], "\\\\\n\\midrule\n")
  body <- paste0(magic_mirror$contents[-1], collapse = "\\\\\n")
  end <- paste0("\\\\\n\\bottomrule\n", "\\end{", magic_mirror$tabular, "}")

  out <- paste0(begin, caption, header, body, end)

  return(out)
}
