#' Put a HTML table into a scrollable box
#'
#' @description This function will put a HTML kable object in a fixed-height,
#' fixed-width or both box and make it scrollable.
#'
#' @param kable_input A HTML kable object
#' @param height A character string indicating the height of the box, e.g. "50px"
#' @param width A character string indicating the width of the box, e.g. "100px"
#'
#' @export
scroll_box <- function(kable_input, height = NULL, width = NULL) {
  kable_attrs <- attributes(kable_input)
  out <- as.character(kable_input)
  box_styles <- c()
  if (!is.null(height)) {
    box_styles <- c(box_styles,
                    paste0("overflow-y: scroll; height:", height, "; "))
  }
  if (!is.null(width)) {
    box_styles <- c(box_styles,
                    paste0("overflow-x: scroll; width:", width, "; "))
  }
  out <- paste0('<div style="', paste(box_styles, collapse = ""), '">',
                out, '</div>')
  out <- structure(out, format = "html",
                   class = "knitr_kable")
  attributes(out) <- kable_attrs
  return(out)
}
