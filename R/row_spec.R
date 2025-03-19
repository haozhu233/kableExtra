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
#' @param monospace A T/F value to control whether the text of the selected row
#' need to be monospaced (verbatim)
#' @param underline A T/F value to control whether the text of the selected row
#' need to be underlined
#' @param strikeout A T/F value to control whether the text of the selected row
#' need to be struck out.
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
#' @param hline_after T/F. A replicate of `hline.after` in xtable. It
#' adds a hline after the row
#' @param extra_latex_after Extra LaTeX text to be added after the row. Similar
#' with `add.to.row` in xtable
#'
#' @examples
#' \dontrun{
#' x <- knitr::kable(head(mtcars), "html")
#' row_spec(x, 1:2, bold = TRUE, italic = TRUE)
#' }
#'
#' @export
row_spec <- function(kable_input, row,
                     bold = FALSE, italic = FALSE, monospace = FALSE,
                     underline = FALSE, strikeout = FALSE,
                     color = NULL, background = NULL, align = NULL,
                     font_size = NULL, angle = NULL, extra_css = NULL,
                     hline_after = FALSE, extra_latex_after = NULL,
                     needs_parsing = TRUE) {
  if (!is.numeric(row)) {
    stop("row must be numeric. ")
  }
  kable_format <- attr(kable_input, "format")
  if (kable_format %in% c("pipe", "markdown")) {
    kable_input <- md_table_parser(kable_input)
    kable_format <- attr(kable_input, "format")
  }
  if (!kable_format %in% c("html", "latex")) {
    warning("Please specify format in kable. kableExtra can customize either ",
            "HTML or LaTeX outputs. See https://haozhu233.github.io/kableExtra/ ",
            "for details.")
    return(kable_input)
  }
  if (kable_format == "html") {
    return(row_spec_html(kable_input, row, bold, italic, monospace,
                         underline, strikeout,
                         color, background, align, font_size, angle,
                         extra_css))
  }
  if (kable_format == "latex") {
    if (needs_parsing)
      parsed <- kable_to_parsed(kable_input)
    else
      parsed <- kable_input
    res <- row_spec_latex(parsed, row, bold, italic, monospace,
                          underline, strikeout,
                          color, background, align, font_size, angle,
                          hline_after, extra_latex_after)
    if (needs_parsing )
      return(parsed_to_kable(res, kable_input))
    else
      return(res)
  }
}

row_spec_html <- function(kable_input, row, bold, italic, monospace,
                          underline, strikeout,
                          color, background, align, font_size, angle,
                          extra_css) {
  kable_attrs <- attributes(kable_input)
  important_nodes <- read_kable_as_xml(kable_input)
  body_node <- important_nodes$body
  kable_xml <- important_nodes$table

  if (!is.null(align)) {
    if (align %in% c("l", "c", "r")) {
      align <- switch(align, r = "right", c = "center", l = "left")
    }
  }

  if (0 %in% row) {
    kable_thead <- xml_tpart(kable_xml, "thead")
    if (is.null(kable_thead))
      warning("No row 0 found.")
    else {
      original_header_row <- xml_child(kable_thead, length(xml_children(kable_thead)))
      for (theader_i in 1:length(xml_children(original_header_row))) {
        target_header_cell <- xml_child(original_header_row, theader_i)
        xml_cell_style(target_header_cell, bold, italic, monospace,
                       underline, strikeout, color, background,
                       align, font_size, angle, extra_css)
      }
    }
    row <- row[row != 0]
  }

  if (length(row) != 0) {
    kable_tbody <- xml_tpart(kable_xml, "tbody")
    if (is.null(kable_tbody))
      warning("No table body found")
    else {
      group_header_rows <- attr(kable_input, "group_header_rows")
      if (!is.null(group_header_rows)) {
        row <- positions_corrector(row, group_header_rows,
                                   length(xml_children(kable_tbody)))
      }

      for (j in row) {
        target_row <- xml_child(kable_tbody, j)
        for (i in 1:length(xml_children(target_row))) {
          target_cell <- xml_child(target_row, i)
          xml_cell_style(target_cell, bold, italic, monospace,
                         underline, strikeout, color, background,
                         align, font_size, angle, extra_css)
        }
      }
    }
  }

  out <- as_kable_xml(body_node)
  attributes(out) <- kable_attrs
  if (!"kableExtra" %in% class(out)) class(out) <- c("kableExtra", class(out))
  return(out)
}

xml_cell_style <- function(x, bold, italic, monospace,
                           underline, strikeout, color, background,
                           align, font_size, angle, extra_css) {
  if (is.na(xml_attr(x, "style"))) {
    xml_attr(x, "style") <- ""
  }
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
  if (underline) {
    xml_attr(x, "style") <- paste0(xml_attr(x, "style"),
                                   "text-decoration: underline;")
  }
  if (strikeout) {
    xml_attr(x, "style") <- paste0(xml_attr(x, "style"),
                                   "text-decoration: line-through;")
  }
  if (!is.null(color)) {
    xml_attr(x, "style") <- paste0(xml_attr(x, "style"),
                                   "color: ", html_color(color), " !important;")
  }
  if (!is.null(background)) {
    xml_attr(x, "style") <- paste0(xml_attr(x, "style"),
                                   "background-color: ",
                                   html_color(background), " !important;")
  }
  if (!is.null(align)) {
    xml_attr(x, "style") <- paste0(xml_attr(x, "style"),
                                   "text-align: ", align, ";")
  }
  if (!is.null(font_size)) {
    if (is.numeric(font_size)) font_size <- paste0(font_size, "px")
    xml_attr(x, "style") <- paste0(xml_attr(x, "style"),
                                   "font-size: ", font_size, ";")
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
    xml_attr(x, "style") <- paste0(xml_attr(x, "style"), enc2utf8(extra_css))
  }
  return(x)
}

row_spec_latex <- function(parsed, row, bold, italic, monospace,
                           underline, strikeout,
                           color, background, align, font_size, angle,
                           hline_after, extra_latex_after) {
  table_info <- magic_mirror_latex(parsed)

  row <- row + table_info$position_offset
  table <- parsed[[table_info$tabularPath]]

  for (i in row) {
    target_row <- table_info$contents[[i]]
    new_row <- latex_new_row_builder(target_row, table_info,
                                     bold, italic, monospace,
                                     underline, strikeout,
                                     color, background, align, font_size, angle,
                                     hline_after, extra_latex_after)

    tableRow(table, i) <- new_row
    table_info$contents[[i]] <- new_row
  }
  parsed[[table_info$tabularPath]] <- table
  update_meta(parsed, table_info)
}

latex_new_row_builder <- function(target_row, table_info,
                                  bold, italic, monospace,
                                  underline, strikeout,
                                  color, background, align, font_size, angle,
                                  hline_after, extra_latex_after) {
  new_row <- latex_row_cells(target_row)
  if (bold) {
    new_row <- lapply(new_row, function(x) {
      latex2("\\textbf", new_block(x))
    })
  }
  if (italic) {
    new_row <- lapply(new_row, function(x) {
      latex2("\\em", new_block(x))
    })
  }
  if (monospace) {
    new_row <- lapply(new_row, function(x) {
      latex2("\\ttfamily", new_block(x))
    })
  }
  if (underline) {
    new_row <- lapply(new_row, function(x) {
      latex2("\\underline", new_block(x))
    })
  }
  if (strikeout) {
    new_row <- lapply(new_row, function(x) {
      latex2("\\sout", new_block(x))
    })
  }
  if (!is.null(color)) {
    if (table_info$tabular == "tabu") {
      warning("Setting full_width = TRUE will turn the table into a tabu ",
              "environment where colors are not really easily configurable ",
              "with this package. Please consider turn off full_width.")
    }
    new_row <- lapply(new_row, function(x) {
      x <- clear_color_latex(x)
      latex2("\\textcolor", latex_color_(color), new_block(x))
    })
  }
  if (!is.null(background)) {
    if (table_info$tabular == "tabu") {
      warning("Setting full_width = TRUE will turn the table into a tabu ",
              "environment where colors are not really easily configable ",
              "with this package. Please consider turn off full_width.")
    }
    if (length(background) > 1)
      stop("Oops, assumed only one color")
    new_row <- lapply(new_row, function(x) {
      x <- clear_color_latex(x, background = TRUE)
      latex2("\\cellcolor", latex_color_(background), new_block(x))
    })
  }
  if (!is.null(font_size)) {
    new_row <- lapply(new_row, function(x) {
      latex2("\\begingroup\\fontsize",
                new_block(font_size),
                new_block(as.numeric(font_size) + 2),
                "\\selectfont  ", x, "\\endgroup")
    })
  }
  if (!is.null(align)) {
    align <- rep(align, table_info$ncol)
    if (!is.null(table_info$column_width)) {
      p_align <- switch(align,
                        "l" = "\\raggedright\\arraybackslash",
                        "c" = "\\centering\\arraybackslash",
                        "r" = "\\raggedleft\\arraybackslash")
      p_cols <- as.numeric(sub("column_", "", names(table_info$column_width)))
      for (i in 1:length(p_cols)) {
        align[p_cols[i]] <- paste0(">{", p_align, "}p{",
                                   table_info$column_width[[i]], "}")
      }
    }
    new_row <- lapply(seq_along(new_row), function(i) {
      latex2("\\multicolumn{1}", new_block(align[i]), new_block(new_row[[i]]))
    })
  }

  if (!is.null(angle)) {
    new_row <- lapply(new_row, function(x) {
      latex2("\\rotatebox", new_block(angle), new_block(x))
    })
  }

  new_row <- vector_to_row(new_row, linebreak = FALSE)

  if (!hline_after & is.null(extra_latex_after)) {
    return(new_row)
  } else {
    latex_after <- latex2("\\\\")
    if (hline_after) {
      if (table_info$booktabs) {
        latex_after <- latex2(latex_after, "\n\\midrule")
      } else {
        latex_after <- latex2(latex_after, "\n\\hline")
      }
    }
    if (!is.null(extra_latex_after)) {
      latex_after <- latex2(latex_after, "\n",
                            extra_latex_after)

    }
    return(c(new_row, latex_after))
  }
}
