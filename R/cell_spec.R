#' Specify Cell/Text format
#'
#' @description Specify Cell format before it gets into kable
#'
#' @param x Things to be formatted. It could be a vector of numbers or strings.
#' @param format Either "html" or "latex". It can also be set through
#' `option(knitr.table.format)`, same as `knitr::kable()`.
#' @param bold T/F for font bold.
#' @param italic T/F for font italic.
#' @param monospace T/F for font monospaced (verbatim)
#' @param underline A T/F value to control whether the text of the selected row
#' need to be underlined
#' @param strikeout A T/F value to control whether the text of the selected row
#' need to be struck out.
#' @param color A character string for text color. Here please pay
#' attention to the differences in color codes between HTML and LaTeX.
#' @param background A character string for background color. Here please
#' pay attention to the differences in color codes between HTML and LaTeX. Also
#' note that in HTML, background defined in cell_spec won't cover the whole
#' cell.
#' @param align A character string for cell alignment. For HTML, possible
#' values could be `l`, `c`, `r` plus `left`, `center`, `right`, `justify`,
#' `initial` and `inherit` while for LaTeX, you can only choose
#' from `l`, `c` & `r`.
#' @param font_size A numeric input for font size. For HTML, you can also use
#' options including `xx-small`, `x-small`, `small`, `medium`, `large`,
#' `x-large`, `xx-large`, `smaller`, `larger`, `initial` and `inherit`.
#' @param angle 0-360, degree that the text will rotate. Can be a vector.
#' @param tooltip A vector of strings to be displayed as tooltip.
#' Obviously, this feature is only available in HTML. Read the package
#' vignette to see how to use bootstrap tooltip css to improve the loading
#' speed and look.
#' @param popover Similar with tooltip but can hold more contents. The best way
#' to build a popover is through `spec_popover()`. If you only provide a text
#' string, it will be used as content. Note that You have to enable this
#' bootstrap module manually. Read the package vignette to see how.
#' @param link A vector of strings for url links. Can be used together with
#' tooltip and popover.
#' @param new_tab T/F for whether to open up the new link in new tab.
#' @param extra_css Extra css text to be passed into the cell
#' @param class Optional HTML class to apply to the cell
#' @param escape T/F value showing whether special characters should be escaped.
#' @param background_as_tile T/F value indicating if you want to have round
#' cornered tile as background in HTML.
#' @param latex_background_in_cell T/F value. It only takes effect in LaTeX
#' when `background` provided, Default value is `TRUE`. If it's `TRUE`, the
#' background only works in a table cell. If it's `FALSE`, it works outside of a
#' table environment.
#'
#' @examples
#' message(cell_spec("The Title", format = "html", class = "title"))
#' message(cell_spec("The Title", format = "html", underline = TRUE))
#' message(cell_spec("The Title", format = "latex", underline = TRUE))
#'
#' @export
cell_spec <- function(x, format,
                      bold = FALSE, italic = FALSE, monospace = FALSE,
                      underline = FALSE, strikeout = FALSE,
                      color = NULL, background = NULL,
                      align = NULL, font_size = NULL, angle = NULL,
                      tooltip = NULL, popover = NULL, link = NULL,
                      new_tab = FALSE, extra_css = NULL,
                      class = NULL,
                      escape = TRUE,
                      background_as_tile = TRUE,
                      latex_background_in_cell = TRUE) {

  if (missing(format) || is.null(format)) {
    if (knitr::is_latex_output()) {
      format <- "latex"
    } else {
      format <- "html"
    }
  }
  if (tolower(format) == "html") {
    return(cell_spec_html(x, bold, italic, monospace, underline, strikeout,
                          color, background, align, font_size, angle,
                          tooltip, popover, link, new_tab, extra_css,
                          class, escape, background_as_tile))
  }
  if (tolower(format) == "latex") {
    return(cell_spec_latex(x, bold, italic, monospace, underline, strikeout,
                           color, background, align, font_size, angle, escape,
                           latex_background_in_cell))
  }
}

cell_spec_html <- function(x, bold, italic, monospace, underline, strikeout,
                           color, background, align, font_size, angle,
                           tooltip, popover, link, new_tab, extra_css, class,
                           escape, background_as_tile) {
  if (escape) x <- escape_html(x)
  cell_style <- NULL
  cell_style <- paste(cell_style, ifelse(bold, "font-weight: bold;", ""))
  cell_style <- paste(cell_style, ifelse(italic, "font-style: italic;", ""))
  cell_style <- paste(cell_style,
                      ifelse(monospace, "font-family: monospace;", ""))
  cell_style <- paste(cell_style,
                      ifelse(underline, "text-decoration: underline;", ""))
  cell_style <- paste(cell_style,
                      ifelse(strikeout, "text-decoration: line-through;", ""))
  if (!is.null(color)) {
    cell_style <- paste0(cell_style, "color: ", html_color(color),
                         " !important;")
  }
  if (!is.null(background)) {
    cell_style <- paste0(
      cell_style,
      ifelse(background_as_tile, "border-radius: 4px; ", ""),
      "padding-right: 4px; padding-left: 4px; ",
      "background-color: ", html_color(background), " !important;"
    )
  }
  if (!is.null(extra_css)) {
    cell_style <- paste0(cell_style, extra_css)
  }
  if (!is.null(class)) {
    class <- paste0(' class="', class, '" ')
  }
  if (!is.null(align)) {
    cell_style <- paste0(cell_style, "text-align: ", align, ";")
  }
  if (!is.null(font_size)) {
    if (is.numeric(font_size)) font_size <- paste0(font_size, "px")
    cell_style <- paste0(cell_style, "font-size: ", font_size, ";")
  }

  # favor popover over tooltip
  if (!is.null(popover)) {
    if (!inherits(popover, "ke_popover")) popover <- spec_popover(popover)
    tooltip_n_popover <- popover
  } else if (!is.null(tooltip)) {
    if (!inherits(tooltip, "ke_tooltip")) tooltip <- spec_tooltip(tooltip)
    tooltip_n_popover <- tooltip
  } else {
    tooltip_n_popover <- NULL
  }

  # Clean up cell_style
  cell_style <- sub("^ *", "", cell_style)
  cell_style <- sub(" *$", "", cell_style)
  cell_style <- ifelse(cell_style == "",
      "",
      paste0('style="', cell_style, '" '))

  if (!is.null(link)) {
    if (new_tab) {
      target_blank = 'target="_blank" '
    } else {
      target_blank = NULL
    }
    x <- paste0('<a href="', link, '" ', cell_style, target_blank,
                tooltip_n_popover, class, '>', x, '</a>')
  } else {
    x <- paste0('<span ', cell_style,
                tooltip_n_popover, class, '>', x, '</span>')
  }

  # if (!is.null(link)) {
  #   x <- paste0('<a href="', link, '" style="', cell_style, '" ',
  #               tooltip_n_popover, '>', x, '</a>')
  # } else if (!is.null(tooltip_n_popover)) {
  #   x <- paste0('<span style="', cell_style, '" ',
  #               tooltip_n_popover, '>', x, '</span>')
  # } else {
  #
  # }


  if (!is.null(angle)) {
    rotate_css <- paste0("-webkit-transform: rotate(", angle,
                         "deg); -moz-transform: rotate(", angle,
                         "deg); -ms-transform: rotate(", angle,
                         "deg); -o-transform: rotate(", angle,
                         "deg); transform: rotate(", angle,
                         "deg); display: inline-block; ")
    x <- paste0('<span style="', rotate_css, '">', x, '</span>')
  }

  return(x)
}

cell_spec_latex <- function(x, bold, italic, monospace, underline, strikeout,
                            color, background, align, font_size, angle, escape,
                            latex_background_in_cell) {
  if (escape) x <- escape_latex(x)
  x <- sprintf(ifelse(bold, "\\textbf{%s}", "%s"), x)
  x <- sprintf(ifelse(italic, "\\em{%s}", "%s"), x)
  x <- sprintf(ifelse(monospace, "\\ttfamily{%s}", "%s"), x)
  x <- sprintf(ifelse(underline, "\\underline{%s}", "%s"), x)
  x <- sprintf(ifelse(strikeout, "\\sout{%s}", "%s"), x)
  if (!is.null(color)) {
    color <- latex_color(color, escape = FALSE)
    x <- paste0("\\textcolor", color, "{", x, "}")
  }
  if (!is.null(background)) {
    background <- latex_color(background, escape = FALSE)
    background_env <- ifelse(latex_background_in_cell, "cellcolor", "colorbox")
    x <- paste0("\\", background_env, background, "{", x, "}")
  }
  if (!is.null(font_size)) {
    x <- paste0("\\bgroup\\fontsize{", font_size, "}{", as.numeric(font_size) + 2,
           "}\\selectfont ", x, "\\egroup{}")
  }
  if (!is.null(angle)) x <- paste0("\\rotatebox{", angle, "}{", x, "}")
  if (!is.null(align)) x <- paste0("\\multicolumn{1}{", align, "}{", x, "}")
  return(x)
}

#' @rdname cell_spec
#' @export
text_spec <- function(x, format,
                      bold = FALSE, italic = FALSE, monospace = FALSE,
                      underline = FALSE, strikeout = FALSE,
                      color = NULL, background = NULL,
                      align = NULL, font_size = NULL, angle = NULL,
                      tooltip = NULL, popover = NULL, link = NULL,
                      new_tab = FALSE, extra_css = NULL,
                      escape = TRUE,
                      background_as_tile = TRUE,
                      latex_background_in_cell = FALSE) {
  cell_spec(x, format, bold, italic, monospace, underline, strikeout,
            color, background, align,
            font_size, angle, tooltip, popover, link, new_tab,
            extra_css, escape, background_as_tile,
            latex_background_in_cell)
}
