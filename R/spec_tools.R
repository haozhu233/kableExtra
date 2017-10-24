#' Generate viridis Color code for continuous values
#'
#' @inheritParams viridisLite::viridis
#' @param x continuous vectors of values
#' @param na_color color code for NA values
#' @export
spec_color <- function(x, alpha = 1, begin = 0, end = 1,
                       direction = 1, option = "D",
                       na_color = "#BBBBBB") {
  x <- round(rescale(x, c(1, 256)))
  color_code <- viridisLite::viridis(256, alpha, begin, end, direction, option)[x]
  color_code[is.na(color_code)] <- na_color
  return(color_code)
}

html_color_ <- function(color) {
  if (substr(color, 1, 1) != "#" | nchar(color) != 9) return(color)
  rgba_code <- col2rgb(color, alpha = TRUE)
  rgba_code[4] <- round(rgba_code[4] / 255, 2)
  return(paste0("rgba(", paste(rgba_code, collapse = ", "), ")"))
}

html_color <- function(colors) {
  colors <- as.character(colors)
  sapply(colors, html_color_)
}

latex_color_ <- function(color) {
  if (substr(color, 1, 1) != "#") {
    return(paste0("{", color, "}"))
  } else {
    color <- sub("#", "", color)
    if (nchar(color) == 8) color <- substr(color, 1, 6)
    return(paste0("[HTML]{", color, "}"))
  }
}
latex_color <- function(colors) {
  colors <- as.character(colors)
  sapply(colors, latex_color_)
}

#' Generate common font size for continuous values
#'
#' @param x continuous vectors of values
#' @param begin Smalles font size to be used. Default is 10.
#' @param end Largest font size. Default is 20.
#' @param na_font_size font size for NA values
#' @export
spec_font_size <- function(x, begin = 8, end = 16, na_font_size = 12) {
  x <- round(rescale(x, c(begin, end)))
  x[is.na(x)] <- na_font_size
  return(x)
}

#' Generate rotation angle for continuous values
#'
#' @param x continuous vectors of values
#' @param begin Smallest degree to rotate. Default is 0
#' @param end Largest degree to rotate. Default is 359.
#' @export
spec_angle <- function(x) {
  x <- round(rescale(x, c(0, 359)))
  x[is.na(x)] <- 0
  return(x)
}

#' Setup bootstrap tooltip
#'
#' @param title text for hovering message
#' @param position How the tooltip should be positioned. Possible values are
#' `right`(default), `top`, `bottom`, `left` & `auto`.
#'
#' @export
spec_tooltip <- function(title, position = "right") {
  position <- match.arg(position, c("right", "bottom", "top", "left", "auto"))
  tooltip_options <- paste(
    'data-toggle="tooltip"',
    paste0('data-placement="', position, '"'),
    # ifelse(as_html, 'data-html="true"', NULL),
    paste0('title="', title, '"'))
  class(tooltip_options) <- "ke_tooltip"
  return(tooltip_options)
}

#' Setup bootstrap popover
#'
#' @param content content for pop-over message
#' @param title title for pop-over message.
#' @param trigger Controls how the pop-over message should be triggered.
#' Possible values include `hover` (default), `click`, `focus` and `manual`.
#' @param position How the tooltip should be positioned. Possible values are
#' `right`(default), `top`, `bottom`, `left` & `auto`.
#'
#' @export
spec_popover <- function(content = NULL, title = NULL,
                         trigger = "hover", position = "right") {
  trigger <- match.arg(trigger, c("hover", "click", "focus", "manual"),
                       several.ok = TRUE)
  position <- match.arg(position, c("bottom", "top", "left", "right", "auto"),
                        several.ok = TRUE)
  popover_options <- paste(
    'data-toggle="popover"',
    paste0('data-trigger="', trigger, '"'),
    paste0('data-placement="', position, '"'),
    ifelse(!is.null(title), paste0('title="', title, '"'), ""),
    paste0('data-content="', content, '"'))
  class(popover_options) <- "ke_popover"
  return(popover_options)
}
