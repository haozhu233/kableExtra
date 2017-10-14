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
#' @param angle 0-360, degree that the text will rotate. Can be a vector.
#' @param hover_message A vector of strings to be displayed as hover message.
#' Of course, this feature is nly available in HTML.
#' @param background_as_tile T/F value indicating if you want to have round
#' cornered tile as background.
#'
#' @export
cell_spec <- function(x, format,
                      bold = F, italic = F, monospace = F,
                      color = NULL, background = NULL,
                      align = NULL, font_size = NULL, angle = NULL,
                      hover_message = NULL, background_as_tile = TRUE) {

  if (missing(format) || is.null(format)) format = getOption('knitr.table.format')
  if (is.null(format)) {
    message("Setting cell_spec format as html")
    format <- "html"
  }

  if (tolower(format) == "html") {
    return(cell_spec_html(x, bold, italic, monospace,
                          color, background, align, font_size, angle,
                          hover_message, background_as_tile))
  }
  if (tolower(format) == "latex") {
    return(cell_spec_latex(x, bold, italic, monospace,
                           color, background, align, angle))
  }
}

cell_spec_html <- function(x, bold, italic, monospace,
                           color, background, align, font_size, angle,
                           hover_message, background_as_tile) {
  cell_style <- NULL
  if (bold) cell_style <- paste(cell_style,"font-weight: bold;")
  if (italic) cell_style <- paste(cell_style, "font-style: italic;")
  if (monospace) cell_style <- paste(cell_style, "font-family: monospace;")
  if (!is.null(color)) {
    cell_style <- paste0(cell_style, "color: ", html_color(color), ";")
  }
  if (!is.null(background)) {
    cell_style <- paste0(
      cell_style,
      ifelse(background_as_tile, "border-radius: 4px; ", ""),
      "padding-right: 4px; padding-left: 4px; ",
      "background-color: ", html_color(background), ";"
    )
  }
  if (!is.null(align)) {
    cell_style <- paste0(cell_style, "text-align: ", align, ";")
  }
  if (!is.null(font_size)) {
    cell_style <- paste0(cell_style, "font-size: ", font_size, "px;")
  }
  if (!is.null(angle)) {
    cell_style <- paste0(cell_style,
                         "-webkit-transform: rotate(", angle,
                         "deg); -moz-transform: rotate(", angle,
                         "deg); -ms-transform: rotate(", angle,
                         "deg); -o-transform: rotate(", angle,
                         "deg); transform: rotate(", angle,
                         "deg);")
  }

  if (!is.null(hover_message)) {
    hover_message <- gsub("\n", "&#013;", hover_message)
    hover_message <- paste0("data-toggle='tooltip' title='", hover_message, "'")
  }
  out <- paste0(
    '<div style="', cell_style, '"', hover_message, '>', x, '</div>'
  )
  return(out)
}

cell_spec_latex <- function(x, bold, italic, monospace,
                            color, background, align, angle) {
  if (bold) x <- paste0("\\bfseries{", x, "}")
  if (italic) x <-paste0("\\em{", x, "}")
  if (monospace) x <- paste0("\\ttfamily{", x, "}")
  if (!is.null(color)) {
    color <- latex_color(color)
    x <- paste0("\\textcolor", color, "{", x, "}")
  }
  if (!is.null(background)) {
    background <- latex_color(background)
    x <- paste0("\\cellcolor", background, "{", x, "}")
  }
  if (!is.null(align)) x <- paste0("\\multicolumn{1}{", align, "}{", x, "}")
  if (!is.null(angle)) x <- paste0("\\rotatebox{", angle, "}{", x, "}")
  return(x)
}
