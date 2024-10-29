#' Render the table as an format-independent image and use it in R Markdown
#'
#' @description This function generates a temporary png file using `save_kable`
#' and then try to put it in an R Markdown document using
#' `knitr::include_graphics`.
#'
#' @param x kable input. Either HTML or LaTeX
#' @param width Image width in inches. (1 inch = 2.54 cm)
#' @param height Image height in inches. (1 inch = 2.54 cm)
#' @param file By default, as_image saves to an temp file, which works for
#' normal R Markdown. However if you are using things like xaringan, which can't
#' be a standalone html, you can specify this file be the path you need, e.g.
#' `"img/something.png"`
#'
#' @param ... Additional arguments passed to save_kable.
#'
#' @examples
#' \dontrun{
#' library(kableExtra)
#'
#' kable(mtcars, "latex", booktabs = T) %>%
#' kable_styling(latex_options = c("striped", "scale_down")) %>%
#' row_spec(1, color = "red") %>%
#' as_image()
#' }
#' @export
as_image <- function(x, width = NULL, height = NULL, file = NULL, ...) {
  if (is.null(width) + is.null(height) == 0) {
    message("Both width and height were defined. Use width only by default. ")
    height <- NULL
  }

  if (is.null(file)) {
    temp_png <- tempfile(fileext = ".png")
  } else {
    temp_png <- file
  }


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


