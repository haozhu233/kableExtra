#' Specify the look of the selected column
#'
#' @description This function allows users to select a column and then specify
#' its look. Right now it supports the following three properties: column width,
#' bold text and italic text.
#'
#' @param kable_input Output of `knitr::kable()` with `format` specified
#' @param column A numeric value indicating which column to be selected. When
#' you do the counting, ignore the extra header columns you added through
#' add_header_left.
#' @param width A character string telling HTML & LaTeX how wide the column
#' needs to be, e.g. "10cm", "3in" or "30em".
#' @param bold A T/F value to control whether the text of the selected column
#' need to be bolded.
#' @param italic A T/F value to control whether the text of the selected column
#' need to be emphasized.
#' @param monospace A T/F value to control whether the text of the selected column
#' need to be monospaced (verbatim)
#' @param color A character string for column text color. Here please pay
#' attention to the differences in color codes between HTML and LaTeX.
#' @param background A character string for column background color. Here please
#' pay attention to the differences in color codes between HTML and LaTeX.
#' @param border_left A logical variable indicating whether there should be a
#' border line on the left of the selected column. In HTML, you can also pass
#' in a character string for the CSS of the border line
#' @param border_right A logical variable indicating whether there should be a
#' border line on the right of the selected column. In HTML, you can also pass
#' in a character string for the CSS of the border line
#'
#' @examples x <- knitr::kable(head(mtcars), "html")
#' column_spec(x, 1, width = "20em", bold = TRUE, italic = TRUE)
#'
#' @export
column_spec <- function(kable_input, column,
                        width = NULL, bold = FALSE, italic = FALSE,
                        monospace = FALSE, color = NULL, background = NULL,
                        border_left = FALSE, border_right = FALSE) {
  if (!is.numeric(column)) {
    stop("column must be a numeric value")
  }
  kable_format <- attr(kable_input, "format")
  if (!kable_format %in% c("html", "latex")) {
    message("Currently generic markdown table using pandoc is not supported.")
    return(kable_input)
  }
  if (kable_format == "html") {
    return(column_spec_html(kable_input, column, width,
                            bold, italic, monospace,
                            color, background,
                            border_left, border_right))
  }
  if (kable_format == "latex") {
    return(column_spec_latex(kable_input, column, width,
                             bold, italic, monospace,
                             color, background,
                             border_left, border_right))
  }
}

column_spec_html <- function(kable_input, column, width,
                             bold, italic, monospace,
                             color, background,
                             border_left, border_right) {
  kable_attrs <- attributes(kable_input)
  kable_xml <- read_kable_as_xml(kable_input)
  kable_tbody <- xml_tpart(kable_xml, "tbody")

  group_header_rows <- attr(kable_input, "group_header_rows")
  if (is.null(kable_attrs$column_adjust)) {
    all_contents_rows <- seq(1, length(xml_children(kable_tbody)))
    all_contents_array <- rep(column, length(all_contents_rows))
  } else {
    column <- column + kable_attrs$column_adjust$count
    all_contents_array <- colSums(kable_attrs$column_adjust$matrix[1:column, ])
    all_contents_rows <- which(all_contents_array != 0 &
                                 kable_attrs$column_adjust$matrix[column, ])
  }

  if (!is.null(group_header_rows)) {
    all_contents_rows <- all_contents_rows[!all_contents_rows %in%
                                             group_header_rows]
  }

  # Border css
  border_l_css <- "1px solid"
  border_r_css <- "1px solid"
  if (is.character(border_left)) {
    border_l_css <- border_left
    border_left <- T
  }
  if (is.character(border_right)) {
    border_r_css <- border_right
    border_right <- T
  }

  for (i in all_contents_rows) {
    target_cell <- xml_child(xml_child(kable_tbody, i), all_contents_array[i])
    if (!is.null(width)) {
      xml_attr(target_cell, "style") <- paste0(xml_attr(target_cell, "style"),
                                               "width: ", width, "; ")
    }
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
    if (border_left) {
      xml_attr(target_cell, "style") <- paste0(xml_attr(target_cell, "style"),
                                               "border-left:", border_l_css, ";")
    }
    if (border_right) {
      xml_attr(target_cell, "style") <- paste0(xml_attr(target_cell, "style"),
                                               "border-right:", border_r_css, ";")
    }
  }
  out <- as_kable_xml(kable_xml)
  attributes(out) <- kable_attrs
  return(out)
}

column_spec_latex <- function(kable_input, column, width,
                              bold, italic, monospace,
                              color, background,
                              border_left, border_right) {
  table_info <- magic_mirror(kable_input)
  if (!is.null(table_info$collapse_rows)) {
    message("Usually it is recommended to use column_spec before collapse_rows,",
            " especially in LaTeX, to get a desired result. ")
  }
  align_collapse <- ifelse(table_info$booktabs, "", "\\|")
  kable_align_old <- paste(table_info$align_vector, collapse = align_collapse)

  table_info$align_vector[column] <- latex_column_align_builder(
    table_info$align_vector_origin[column], width, bold, italic, monospace,
    color, background, border_left, border_right)

  kable_align_new <- paste(table_info$align_vector, collapse = align_collapse)

  out <- sub(kable_align_old, kable_align_new,
             enc2utf8(as.character(kable_input)),
             perl = T)
  out <- structure(out, format = "latex", class = "knitr_kable")
  if (!is.null(width)) {
    if (is.null(table_info$column_width)) {
      table_info$column_width <- list()
    }
    table_info$column_width[[paste0("column_", column)]] <- width
  }
  attr(out, "kable_meta") <- table_info
  return(out)
}

latex_column_align_builder <- function(x, width, bold, italic, monospace,
                                       color, background,
                                       border_left, border_right) {
  extra_align <- ""
  if (!is.null(width)) {
    extra_align <- switch(x,
                          "l" = "\\\\raggedright\\\\arraybackslash",
                          "c" = "\\\\centering\\\\arraybackslash",
                          "r" = "\\\\raggedleft\\\\arraybackslash")
    x <- paste0("p\\{", width, "\\}")
  }

  if (!is.null(color)) {
    color <- sprintf("\\\\color{%s}", color)
  }

  if (!is.null(color)) {
    background <- sprintf("\\\\columncolor{%s}", background)
  }

  latex_array_options <- c("\\\\bfseries", "\\\\em", "\\\\ttfamily")[
    c(bold, italic, monospace)]
  latex_array_options <- c(latex_array_options, extra_align,
                           color, background)
  latex_array_options <- paste0(
    "\\>\\{", paste(latex_array_options, collapse = ""), "\\}"
  )
  x <- paste0(latex_array_options, x)
  if (border_left) {
    x <- paste0("|", x)
  }
  if (border_right) {
    x <- paste0(x, "|")
  }

  return(x)
}
