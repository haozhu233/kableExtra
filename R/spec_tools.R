#' Generate viridis or other color code for continuous values
#'
#' @param x continuous vectors of values
#' @param alpha The alpha transparency, a number in \[0,1\],
#' @param begin The (corrected) hue in \[0,1\] at which the color map begins.
#' @param end The (corrected) hue in \[0,1\] at which the color map ends.
#' @param direction Sets the order of colors in the scale. If 1, the default,
#' colors are ordered from darkest to lightest. If -1, the order of colors is
#' reversed.
#' @param option A character string indicating the color map option to use.
#' Eight options are available: "magma" (or "A"), "inferno" (or "B"),
#' "plasma" (or "C"), "viridis" (or "D"), "cividis" (or "E"),
#' "rocket" (or "F"), "mako" (or "G") and "turbo" (or "H").
#' @param na_color color code for NA values
#' @param scale_from input range (vector of length two). If not given,
#' is calculated from the range of x
#' @param palette The palette to use as a character vector of colors.  If
#' this is specified, parameters other than `x`, `na_color` and `scale_from`
#' are ignored.
#' @export
spec_color <- function(x, alpha = 1, begin = 0, end = 1,
                       direction = 1, option = "D",
                       na_color = "#BBBBBB", scale_from = NULL,
                       palette = viridisLite::viridis(
                         256, alpha, begin, end, direction, option
                         )) {
  n <- length(palette)
  if (is.null(scale_from)) {
    x <- round(rescale(x, c(1, n)))
  } else {
    x <- round(rescale(x, to = c(1, n),
                       from = scale_from))
  }

  color_code <- palette[x]
  color_code[is.na(color_code)] <- na_color
  return(color_code)
}

html_color_ <- function(color) {
  # HTML colors are a subset of R colors, not including
  # numbered versions like darkgoldenrod2 (issue #726)
  if (substr(color, 1, 1) != "#" &&
      !grepl("[[:digit:]]", color) )
    return(color)

  # 2024-01-23 Hao: Move it to a try catch flavor to catch some exception cases.
  tryCatch({
    rgba_code <- col2rgb(color, alpha = TRUE)
    # issue #902, alpha should be [0,1]
    rgba_code[4,] <- round(rgba_code[4,] / 255, 2)
    return(paste0("rgba(", paste(rgba_code, collapse = ", "), ")"))
  },
    error = function(e) {return(as.character(color))}
  )
}

html_color <- function(colors) {
  colors <- trimws(gsub("\\!important", "", as.character(colors)))
  sapply(colors, html_color_)
}

latex_color_ <- function(color) {
  if (substr(color, 1, 1) != "#") {
    return(paste0("\\{", color, "\\}"))
  } else {
    color <- sub("#", "", color)
    if (nchar(color) == 8) color <- substr(color, 1, 6)
    return(paste0("\\[HTML\\]\\{", color, "\\}"))
  }
}

latex_color__ <- function(color) {
  if (substr(color, 1, 1) != "#") {
    return(paste0("{", color, "}"))
  } else {
    color <- sub("#", "", color)
    if (nchar(color) == 8) color <- substr(color, 1, 6)
    return(paste0("[HTML]{", color, "}"))
  }
}
latex_color <- function(colors, escape = TRUE) {
  colors <- as.character(colors)
  if (escape) {
    return(sapply(colors, latex_color_))
  } else {
    return(sapply(colors, latex_color__))
  }

}

#' Generate common font size for continuous values
#'
#' @param x continuous vectors of values
#' @param begin Smallest font size to be used. Default is 10.
#' @param end Largest font size. Default is 20.
#' @param na_font_size font size for NA values
#' @param scale_from input range (vector of length two). If not given,
#' is calculated from the range of x
#' @export
spec_font_size <- function(x, begin = 8, end = 16, na_font_size = 12,
                           scale_from = NULL) {
  if (is.null(scale_from)) {
    x <- round(rescale(x, c(begin, end)))
  } else {
    x <- round(rescale(x, to = c(begin, end),
                       from = scale_from))
  }
  x[is.na(x)] <- na_font_size
  return(x)
}

#' Generate rotation angle for continuous values
#'
#' @param x continuous vectors of values
#' @param begin Smallest degree to rotate. Default is 0
#' @param end Largest degree to rotate. Default is 359.
#' @param scale_from input range (vector of length two). If not given,
#' is calculated from the range of x
#' @export
spec_angle <- function(x, begin, end, scale_from = NULL) {
  if (is.null(scale_from)) {
    x <- round(rescale(x, c(begin, end)))
  } else {
    x <- round(rescale(x, to = c(begin, end),
                       from = scale_from))
  }
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
  position <- match.arg(position, c("right", "bottom", "top", "left", "auto"),
                        several.ok = TRUE)
  tooltip_options <- paste(
    'data-toggle="tooltip" data-container="body"',
    paste0('data-placement="', position, '"'),
    # ifelse(as_html, 'data-html="true"', NULL),
    paste0('title="', title, '"'))
  tooltip_options_list <- list(
    'data-toggle' = 'tooltip',
    'data-container' = 'body',
    'data-placement' = position,
    'title' = if(is.null(title)) '' else title
  )
  class(tooltip_options) <- "ke_tooltip"
  attr(tooltip_options, 'list') <- tooltip_options_list
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
    'data-toggle="popover" data-container="body"',
    paste0('data-trigger="', trigger, '"'),
    paste0('data-placement="', position, '"'),
    ifelse(!is.null(title), paste0('title="', title, '"'), ""),
    paste0('data-content="', content, '"'))
  popover_options_list <- list(
    'data-toggle' = 'popover',
    'data-container' = 'body',
    'data-trigger' = trigger,
    'data-placement' = position,
    'data-content' = content
  )
  if (!is.null(title)) {
    popover_options_list['title'] <- title
  }
  class(popover_options) <- "ke_popover"
  attr(popover_options, 'list') <- popover_options_list
  return(popover_options)
}

#' Setup image path, size, etc
#'
#' @description Users can directly provide image file path to column spec.
#' However, if you need to specify the size of the image, you will need this
#' function.
#'
#' @param path file path(s)
#' @param width image width in pixel
#' @param height image height in pixel
#' @param res image resolution.
#' @param svg_text If you have the raw text for SVG. Put them here
#'
#' @export
spec_image <- function(path, width, height, res = 300, svg_text = NULL) {
  if (length(path) > 1) {
    return(lapply(path, function(p) {
      return(spec_image(p, width, height, res, svg_text))
    }))
  }
  if (!is.null(svg_text)) {
    out <- list(path = NULL, dev = NULL, type = "image",
                width = NULL, height = NULL, res = NULL,
                svg_text = svg_text)
    class(out) <- "kableExtraInlinePlots"
    return(out)
  }
  out <- list(path = path, dev = "external", type = "image",
              width = width, height = height, res = res,
              svg_text = svg_text)
  class(out) <- "kableExtraInlinePlots"
  return(out)
}
