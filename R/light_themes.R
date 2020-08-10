#' Alternative HTML themes
#'
#' @description kableExtra uses the built-in bootstrap themes by default in
#' `kable_styling()`. Alternatively, you can use a customized table themes for
#' your table. This `lightable` table style sheet comes with three formats,
#' namely `lightable-minimal`, `lightable-classic` and `lightable-material` with
#' `hover` and `striped` options.
#'
#' @param kable_input A HTML kable object.
#' @param striped T/F for adding striped rows.
#' @param hover T/F for adding hover effects.
#' @param ... Everything else you need to specify in `kable_styling`.
#'
#' @export
kable_classic <- function(kable_input, striped = FALSE,
                          hover = FALSE, ...) {
  light_class <- "lightable-classic"
  if (striped) {
    light_class <- paste(light_class, "lightable-striped")
  }
  if (hover) {
    light_class <- paste(light_class, "lightable-hover")
  }
  kable_styling(kable_input, "none", lightable_class = light_class, ...)
}

#' @rdname kable_classic
#' @export
kable_minimal <- function(kable_input, striped = FALSE,
                          hover = FALSE, ...) {
  light_class <- "lightable-minimal"
  if (striped) {
    light_class <- paste(light_class, "lightable-striped")
  }
  if (hover) {
    light_class <- paste(light_class, "lightable-hover")
  }
  kable_styling(kable_input, "none", lightable_class = light_class, ...)
}

#' @rdname kable_classic
#' @export
kable_material <- function(kable_input, striped = FALSE,
                           hover = FALSE, ...) {
  light_class <- "lightable-material"
  if (striped) {
    light_class <- paste(light_class, "lightable-striped")
  }
  if (hover) {
    light_class <- paste(light_class, "lightable-hover")
  }
  kable_styling(kable_input, "none", lightable_class = light_class, ...)
}
