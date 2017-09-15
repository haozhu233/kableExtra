#' Specify the look of the selected row
#'
#' @description This function allows users to select a row and then specify
#' its look. Right now it supports the following two properties: bold text and
#' italic text.
#'
#' @param kable_input Output of `knitr::kable()` with `format` specified
#' @param row A numeric value or vector indicating which row(s) to be selected. You don't
#' need to count in header rows or group labeling rows.
#' @param bold A T/F value to control whether the text of the selected row
#' need to be bolded.
#' @param italic A T/F value to control whether the text of the selected row
#' need to be emphasized.
#' @param monospace A T/F value to control whether the text of the selected column
#' need to be monospaced (verbatim)
#' @param color A character string for column text color. Here please pay
#' attention to the differences in color codes between HTML and LaTeX.
#' @param background A character string for column background color. Here please
#' pay attention to the differences in color codes between HTML and LaTeX.
#'
#' @examples x <- knitr::kable(head(mtcars), "html")
#' row_spec(x, 1:2, bold = TRUE, italic = TRUE)
#'
#' @export
row_spec <- function(kable_input, row,
                     bold = FALSE, italic = FALSE, monospace = FALSE,
                     color = NULL, background = NULL) {
  if (!is.numeric(row)) {
    stop("row must be numeric. ")
  }
  kable_format <- attr(kable_input, "format")
  if (!kable_format %in% c("html", "latex")) {
    message("Currently generic markdown table using pandoc is not supported.")
    return(kable_input)
  }
  if (kable_format == "html") {
    return(row_spec_html(kable_input, row, bold, italic, monospace,
                         color, background))
  }
  if (kable_format == "latex") {
    return(row_spec_latex(kable_input, row, bold, italic, monospace,
                          color, background))
  }
}

row_spec_html <- function(kable_input, row, bold, italic, monospace,
                          color, background) {
  kable_attrs <- attributes(kable_input)
  kable_xml <- read_kable_as_xml(kable_input)
  kable_tbody <- xml_tpart(kable_xml, "tbody")

  group_header_rows <- attr(kable_input, "group_header_rows")
  if (!is.null(group_header_rows)) {
      row <- positions_corrector(row, group_header_rows,
                                 length(xml_children(kable_tbody)))
  }

  for (j in row) {
    target_row <- xml_child(kable_tbody, j)
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
      if (monospace) {
        xml_attr(target_cell, "style") <- paste0(xml_attr(target_cell, "style"),
                                                 "font-family: monospace;")
      }
      if (!is.null(color)) {
        xml_attr(target_cell, "style") <- paste0(xml_attr(target_cell, "style"),
                                                 "color: ", color, ";")
      }
      if (!is.null(background)) {
        xml_attr(target_cell, "style") <- paste0(xml_attr(target_cell, "style"),
                                                 "background-color: ",
                                                 background, ";")
      }
    }
  }

  out <- as_kable_xml(kable_xml)
  attributes(out) <- kable_attrs
  return(out)
}

row_spec_latex <- function(kable_input, row, bold, italic, monospace,
                           color, background) {
  table_info <- magic_mirror(kable_input)
  out <- enc2utf8(as.character(kable_input))
  row <- row + 1
  for (i in row) {
    target_row <- table_info$contents[i]
    new_row <- latex_new_row_builder(target_row, bold, italic, monospace,
                                     color, background)
    out <- sub(target_row, new_row, out, perl = T)
  }

  out <- structure(out, format = "latex", class = "knitr_kable")
  attr(out, "kable_meta") <- table_info
  return(out)
}

latex_new_row_builder <- function(target_row, bold, italic, monospace,
                                  color, background) {
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
  if (monospace) {
    new_row <- lapply(new_row, function(x) {
      paste0("\\\\ttfamily{", x, "}")
    })
  }

  if (!is.null(color)) {
    new_row <- lapply(new_row, function(x) {
      paste0("\\\\textcolor{", color, "}{", x, "}")
    })
  }
  new_row <- paste(unlist(new_row), collapse = " & ")

  if (!is.null(background)) {
    new_row <- paste0("\\\\rowcolor{", background, "}  ", new_row)
  }

  return(new_row)
}
