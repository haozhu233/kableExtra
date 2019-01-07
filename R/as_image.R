#' Render the table as an format-independent image and use it in rmarkdown
#'
#' @description This function generates a temporary png file using `save_kable`
#' and then try to put it in an rmarkdown document using
#' `knitr::include_graphics`.
#'
#' @param x kable input. Either HTML or LaTeX
#' @param width Image width in inches. (1 inch = 2.54 cm)
#' @param height Image height in inches. (1 inch = 2.54 cm)
#' @param ... Additional arguments passed to save_kable.
#'
#'
#' @export
as_image <- function(x, width = NULL, height = NULL,
                     ...) {
  if (is.null(width) + is.null(height) == 0) {
    message("Both width and height were defined. Use width only by default. ")
    height <- NULL
  }

  temp_png <- tempfile(fileext = ".png")
  temp_img <- save_kable(x = x, file = temp_png, ...)

  img_dpi <- 300

  if (is.null(width) + is.null(height) <= 1 & is.null(attr(temp_img, "info"))) {
    warning("You need to install magick in order to use width/height in ",
            "as_image. ")
  } else {
    if (!is.null(width)) {
      img_dpi <- attr(temp_img, "info")$width / width
    }
    if (!is.null(height)) {
      img_dpi <- attr(temp_img, "info")$height / height
    }
  }

  include_graphics(temp_png, dpi = img_dpi)
}


