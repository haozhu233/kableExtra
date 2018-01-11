#' Specify the look of the selected row
#'
#' @description This function allows users to select a row and then specify
#' its look. It can also specify the format of the header row when `row` = 0.
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
#' @param color A character string for row text color. For example, "red" or
#' "#BBBBBB".
#' @param background A character string for row background color. Here please
#' pay attention to the differences in color codes between HTML and LaTeX.
#' @param align A character string for cell alignment. For HTML, possible values could
#' be `l`, `c`, `r` plus `left`, `center`, `right`, `justify`, `initial` and `inherit`
#' while for LaTeX, you can only choose from `l`, `c` & `r`.
#' @param font_size A numeric input for font size. For HTML, you can also use
#' options including `xx-small`, `x-small`, `small`, `medium`, `large`,
#' `x-large`, `xx-large`, `smaller`, `larger`, `initial` and `inherit`.
#' @param angle 0-360, degree that the text will rotate.
#' @param extra_css Extra css text to be passed into the cells of the row. Note
#' that it's not for the whole row.
#'
#' @examples x <- knitr::kable(head(mtcars), "html")
#' row_spec(x, 1:2, bold = TRUE, italic = TRUE)
#'
#' @export
row_spec <- function(kable_input, row,
                     bold = FALSE, italic = FALSE, monospace = FALSE,
                     color = NULL, background = NULL, align = NULL,
                     font_size = NULL, angle = NULL, extra_css = NULL) {
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
                         color, background, align, font_size, angle,
                         extra_css))
  }
  if (kable_format == "latex") {
    return(row_spec_latex(kable_input, row, bold, italic, monospace,
                          color, background, align, font_size, angle))
  }
}

row_spec_html <- function(kable_input, row, bold, italic, monospace,
                          color, background, align, font_size, angle,
                          extra_css) {
  kable_attrs <- attributes(kable_input)
  kable_xml <- read_kable_as_xml(kable_input)

  if (!is.null(align)) {
    if (align %in% c("l", "c", "r")) {
      align <- switch(align, r = "right", c = "center", l = "left")
    }
  }

  if (0 %in% row) {
    kable_thead <- xml_tpart(kable_xml, "thead")
    original_header_row <- xml_child(kable_thead, length(xml_children(kable_thead)))
    for (theader_i in 1:length(xml_children(original_header_row))) {
      target_header_cell <- xml_child(original_header_row, theader_i)
      xml_cell_style(target_header_cell, bold, italic, monospace, color, background,
                     align, font_size, angle, extra_css)
    }
    row <- row[row != 0]
  }

  if (length(row) != 0) {
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
        xml_cell_style(target_cell, bold, italic, monospace, color, background,
                       align, font_size, angle, extra_css)
      }
    }
  }

  out <- as_kable_xml(kable_xml)
  attributes(out) <- kable_attrs
  if (!"kableExtra" %in% class(out)) class(out) <- c("kableExtra", class(out))
  return(out)
}

xml_cell_style <- function(x, bold, italic, monospace, color, background,
                           align, font_size, angle, extra_css) {
  if (bold) {
    xml_attr(x, "style") <- paste0(xml_attr(x, "style"),
                                   "font-weight: bold;")
  }
  if (italic) {
    xml_attr(x, "style") <- paste0(xml_attr(x, "style"),
                                   "font-style: italic;")
  }
  if (monospace) {
    xml_attr(x, "style") <- paste0(xml_attr(x, "style"),
                                   "font-family: monospace;")
  }
  if (!is.null(color)) {
    xml_attr(x, "style") <- paste0(xml_attr(x, "style"),
                                   "color: ", color, ";")
  }
  if (!is.null(background)) {
    xml_attr(x, "style") <- paste0(xml_attr(x, "style"),
                                   "background-color: ",
                                   background, ";")
  }
  if (!is.null(align)) {
    xml_attr(x, "style") <- paste0(xml_attr(x, "style"),
                                   "text-align: ", align, ";")
  }
  if (!is.null(font_size)) {
    xml_attr(x, "style") <- paste0(xml_attr(x, "style"),
                                   "font-size: ", font_size, "px;")
  }
  if (!is.null(angle)) {
    xml_attr(x, "style") <- paste0(xml_attr(x, "style"),
                                   "-webkit-transform: rotate(", angle,
                                   "deg); -moz-transform: rotate(", angle,
                                   "deg); -ms-transform: rotate(", angle,
                                   "deg); -o-transform: rotate(", angle,
                                   "deg); transform: rotate(", angle,
                                   "deg);")
  }
  if (!is.null(extra_css)) {
    xml_attr(x, "style") <- paste0(xml_attr(x, "style"), extra_css)
  }
  return(x)
}

row_spec_latex <- function(kable_input, row, bold, italic, monospace,
                           color, background, align, font_size, angle) {
  table_info <- magic_mirror(kable_input)
  out <- enc2utf8(as.character(kable_input))

  if (table_info$duplicated_rows) {
    dup_fx_out <- fix_duplicated_rows_latex(out, table_info)
    out <- dup_fx_out[[1]]
    table_info <- dup_fx_out[[2]]
  }

  row <- row + 1
  for (i in row) {
    target_row <- table_info$contents[i]
    new_row <- latex_new_row_builder(target_row, bold, italic, monospace,
                                     color, background, align, font_size, angle)
    out <- sub(target_row, new_row, out, perl = T)
  }

  out <- structure(out, format = "latex", class = "knitr_kable")
  attr(out, "kable_meta") <- table_info
  return(out)
}

latex_new_row_builder <- function(target_row, bold, italic, monospace,
                                  color, background, align, font_size, angle) {
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
      paste0("\\\\textcolor", latex_color(color), "{", x, "}")
    })
  }
  if (!is.null(font_size)) {
    new_row <- lapply(new_row, function(x) {
      paste0("\\\\begingroup\\\\fontsize{", font_size, "}{",
             as.numeric(font_size) + 2,
             "}\\\\selectfont ", x, "\\\\endgroup")})
  }
  if (!is.null(align)) {
    new_row <- lapply(new_row, function(x) {
      paste0("\\\\multicolumn{1}{", align, "}{", x, "}")
    })
  }

  if (!is.null(angle)) {
    new_row <- lapply(new_row, function(x) {
      paste0("\\\\rotatebox{", angle, "}{", x, "}")
    })
  }

  new_row <- paste(unlist(new_row), collapse = " & ")

  if (!is.null(background)) {
    new_row <- paste0("\\\\rowcolor", latex_color(background), "  ", new_row)
  }

  return(new_row)
}
