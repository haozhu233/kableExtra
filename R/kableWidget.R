#' @import htmlwidgets
#' @export
kableExtraWidget <- function(kable_input, width = NULL, height = NULL) {
  kable_format <- attr(kable_input, "format")
  if (kable_format != "html") {
    stop("kable_as_widget only works for HTML tables.")
  }
  x <- as.character(kable_input)
  htmlwidgets::createWidget("kable_as_widget", x,
                            width = width, height = height)
}
