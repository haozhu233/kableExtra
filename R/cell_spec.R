#' Specify Cell format
#'
#' @description Specify Cell format before it gets into kable
#'
#' @param x Things to be formated. It could be a vector of numbers or strings.
#' @param format Either "html" or "latex". It can also be set through
#' `option(knitr.table.format)`, same as `knitr::kable()`.
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
#' @param align A character string for cell alignment. For HTML, possible values could
#' be `l`, `c`, `r` plus `left`, `center`, `right`, `justify`, `initial` and `inherit`
#' while for LaTeX, you can only choose from `l`, `c` & `r`.
#' @param font_size Only if you want to specify font size locally in HTML.
#' This feature is not available in LaTeX
#' @param angle 0-360, degree that the text will rotate.
#'
#' @export
cell_spec <- function(x, format,
                      bold = F, italic = F, monospace = F,
                      color = NULL, background = NULL,
                      align = NULL, font_size = NULL, angle = NULL) {

  if (missing(format) || is.null(format)) format = getOption('knitr.table.format')
  if (is.null(format)) {
    warning("Output format for cell_formatter was not specified. Using ",
            "html as default. You may consider to set it up via option knitr.table.format.",
            "See ?cell_formatter for details. ")
    return(x)
  }

  if (tolower(format) == "html") {
    return(cell_spec_html(x, bold, italic, monospace,
                          color, background, align, font_size, angle))
  }
  if (tolower(format) == "latex") {
    return(cell_spec_latex(x, bold, italic, monospace,
                           color, background, align, angle))
  }
}

cell_spec_html <- function(x, bold, italic, monospace,
                           color, background, align, font_size, angle) {
  cell_style <- NULL
  if (bold) cell_style <- paste(cell_style,"font-weight: bold;")
  if (italic) cell_style <- paste(cell_style, "font-style: italic;")
  if (monospace) cell_style <- paste(cell_style, "font-family: monospace;")
  if (!is.null(color)) cell_style <- paste0(cell_style, " color: ", color, ";")
  if (!is.null(background)) {
    cell_style <- paste0(cell_style, " border-radius: 4px; padding-right: 2px",
                         "; background-color: ", background, ";")
  }
  if (!is.null(align)) {
    cell_style <- paste0(cell_style, " text-align: ", align, ";")
  }
  if (!is.null(font_size)) {
    cell_style <- paste0(cell_style, " font-size: ", font_size, "px;")
  }
  if (!is.null(angle)) {
    cell_style <- paste0(cell_style,
                         " -webkit-transform: rotate(", angle,
                         "deg); -moz-transform: rotate(", angle,
                         "deg); -ms-transform: rotate(", angle,
                         "deg); -o-transform: rotate(", angle, "deg);")
  }
  out <- paste0(
    '<div style="', cell_style, '">', x, '</div>'
  )
  return(out)
}

cell_spec_latex <- function(x, bold, italic, monospace,
                            color, background, align, angle) {
  if (bold) x <- paste0("\\bfseries{", x, "}")
  if (italic) x <-paste0("\\em{", x, "}")
  if (monospace) x <- paste0("\\ttfamily{", x, "}")
  if (!is.null(color)) x <- paste0("\\textcolor{", color, "}{", x, "}")
  if (!is.null(background)) x <- paste0("\\cellcolor{", background, "}{", x, "}")
  if (!is.null(align)) x <- paste0("\\multicolumn{1}{", align, "}{", x, "}")
  if (!is.null(angle)) x <- paste0("\\rotatebox{", angle, "}{", x, "}")
  return(x)
}
