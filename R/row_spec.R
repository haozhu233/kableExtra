#' Specify the look of the selected row
#'
#' @description This function allows users to select a row and then specify
#' its look. Right now it supports the following two properties: bold text and
#' italic text.
#'
#' @param kable_input Output of `knitr::kable()` with `format` specified
#' @param row A numeric value indicating which row to be selected. You don't
#' need to count in header rows or group labeling rows.
#' @param bold A T/F value to control whether the text of the selected row
#' need to be bolded.
#' @param italic A T/F value to control whether the text of the selected row
#' need to be emphasized.
#'
#' @examples x <- knitr::kable(head(mtcars), "html")
#' row_spec(x, 1, bold = TRUE, italic = TRUE)
#'
#' @export
row_spec <- function(kable_input, row,
                     bold = FALSE, italic = FALSE) {
  if (!is.numeric(row)) {
    stop("row must be a numeric value")
  }
  kable_format <- attr(kable_input, "format")
  if (!kable_format %in% c("html", "latex")) {
    message("Currently generic markdown table using pandoc is not supported.")
    return(kable_input)
  }
  if (kable_format == "html") {
    return(row_spec_html(kable_input, row, bold, italic))
  }
  if (kable_format == "latex") {
    return(row_spec_latex(kable_input, row, bold, italic))
  }
}

row_spec_html <- function(kable_input, row, bold, italic) {
  kable_attrs <- attributes(kable_input)
  kable_xml <- read_xml(as.character(kable_input), options = "COMPACT")
  kable_tbody <- xml_tpart(kable_xml, "tbody")

  group_header_rows <- attr(kable_input, "group_header_rows")
  if (!is.null(group_header_rows)) {
    row <- positions_corrector(row, group_header_rows,
                               length(xml_children(kable_tbody)))
  }

  target_row <- xml_child(kable_tbody, row)

  for (i in 1:length(xml_children(target_row))) {
    target_cell <- xml_child(target_row, i)
    if (bold) {
      xml_attr(target_cell, "style") <- paste0(xml_attr(target_cell, "style"),
                                               "font-weight: bold;")
    }
    if (italic) {
      xml_attr(target_cell, "style") <- paste0(xml_attr(target_cell, "style"),
                                               "font-style: italic;")
    }
  }
  out <- structure(as.character(kable_xml), format = "html",
                   class = "knitr_kable")
  attributes(out) <- kable_attrs
  return(out)
}

row_spec_latex <- function(kable_input, row, bold, italic) {
  table_info <- magic_mirror(kable_input)
  target_row <- table_info$contents[row + 1]
  new_row <- latex_row_cells(target_row)
  if (bold) {
    new_row <- lapply(new_row, function(x) {
      paste0("\\\\bfseries{", x, "}")
    })
  }
  if (italic) {
    new_row <- lapply(new_row, function(x) {
      paste0("\\\\em{", x, "}")
    })
  }
  new_row <- paste(unlist(new_row), collapse = " & ")

  out <- sub(target_row, new_row, as.character(kable_input), perl = T)
  out <- structure(out, format = "latex", class = "knitr_kable")
  attr(out, "original_kable_meta") <- table_info
  return(out)
}
