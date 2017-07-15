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
#'
#' @examples x <- knitr::kable(head(mtcars), "html")
#' column_spec(x, 1, width = "20em", bold = TRUE, italic = TRUE)
#'
#' @export
column_spec <- function(kable_input, column,
                        width = NULL, bold = FALSE, italic = FALSE,
                        monospace = FALSE) {
  if (!is.numeric(column)) {
    stop("column must be a numeric value")
  }
  kable_format <- attr(kable_input, "format")
  if (!kable_format %in% c("html", "latex")) {
    message("Currently generic markdown table using pandoc is not supported.")
    return(kable_input)
  }
  if (kable_format == "html") {
    return(column_spec_html(kable_input, column, width, bold, italic, monospace))
  }
  if (kable_format == "latex") {
    return(column_spec_latex(kable_input, column, width, bold, italic, monospace))
  }
}

column_spec_html <- function(kable_input, column, width, bold, italic, monospace) {
  kable_attrs <- attributes(kable_input)
  kable_xml <- read_xml(as.character(kable_input), options = "COMPACT")
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
  }
  out <- structure(as.character(kable_xml), format = "html",
                   class = "knitr_kable")
  attributes(out) <- kable_attrs
  return(out)
}

column_spec_latex <- function(kable_input, column, width, bold, italic, monospace) {
  table_info <- magic_mirror(kable_input)
  if (!is.null(table_info$collapse_rows)) {
    message("Usually it is recommended to use column_spec before collapse_rows,",
            " especially in LaTeX, to get a desired result. ")
  }
  align_collapse <- ifelse(table_info$booktabs, "", "\\|")
  kable_align_old <- paste(table_info$align_vector, collapse = align_collapse)

  table_info$align_vector[column] <- latex_column_align_builder(
    table_info$align_vector[column], width, bold, italic, monospace)

  kable_align_new <- paste(table_info$align_vector, collapse = align_collapse)

  out <- sub(kable_align_old, kable_align_new, as.character(kable_input),
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

latex_column_align_builder <- function(x, width, bold, italic, monospace) {
  extra_align <- ""
  if (!is.null(width)) {
    extra_align <- switch(x,
                          "l" = "\\\\raggedright\\\\arraybackslash",
                          "c" = "\\\\centering\\\\arraybackslash",
                          "r" = "\\\\raggedleft\\\\arraybackslash")
    x <- paste0("p\\{", width, "\\}")
  }

  if (bold | italic | monospace | extra_align != "") {
    latex_array_options <- c("\\\\bfseries", "\\\\em", "\\\\ttfamily")[
      c(bold, italic, monospace)]
    latex_array_options <- c(latex_array_options, extra_align)
    latex_array_options <- paste0(
      "\\>\\{", paste(latex_array_options, collapse = ""), "\\}"
    )
    x <- paste0(latex_array_options, x)
  }

  return(x)
}
