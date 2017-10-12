#' Generate viridis Color code for continuous values
#'
#' @inheritParams viridisLite::viridis
#' @param x continuous vectors of values
#' @param na_color color code for NA values
#' @export
spec_color <- function(x, alpha = 1, begin = 0, end = 1,
                       direction = 1, option = "D",
                       na_color = "#BBBBBBFF") {
  x <- round(rescale(x, c(1, 256)))
  color_code <- viridisLite::viridis(256, alpha, begin, end, direction, option)[x]
  color_code[is.na(color_code)] <- na_color
  return(color_code)
}

#' Generate common font size for continuous values
#'
#' @param x continuous vectors of values
#' @param begin Smalles font size to be used. Default is 10.
#' @param end Largest font size. Default is 20.
#' @param na_font_size font size for NA values
#' @export
spec_font_size <- function(x, begin = 10, end = 20, na_font_size = "inherit") {
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
