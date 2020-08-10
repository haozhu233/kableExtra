#' Alternative HTML themes
#'
#' @description kableExtra uses the built-in bootstrap themes by default in
#' `kable_styling()`. Alternatively, you can use a customized table themes for
#' your table. This `lightable` table style sheet comes with three formats,
#' namely `lightable-minimal`, `lightable-classic` and `lightable-material` with
#' `hover` and `striped` options.
#'
#' @param kable_input A HTML kable object.
#' @param lightable_options Options to customize lightable. Similar with
#' `bootstrap_options` in `kable_styling`. Choices include `basic`, `striped`
#' and `hover`.
#' @param ... Everything else you need to specify in `kable_styling`.
#'
#' @export
kable_classic <- function(kable_input, lightable_options = "basic", ...) {
  light_class <- "lightable-classic"
  lightable_options <- match.arg(lightable_options,
                                 choices = c("basic", "striped", "hover"),
                                 several.ok = TRUE)
  if ("striped" %in% lightable_options) {
    light_class <- paste(light_class, "lightable-striped")
  }
  if ("hover" %in% lightable_options) {
    light_class <- paste(light_class, "lightable-hover")
  }
  kable_styling(kable_input, "none", lightable_class = light_class, ...)
}

#' @rdname kable_classic
#' @export
kable_minimal <- function(kable_input, lightable_options = "basic", ...) {
  light_class <- "lightable-minimal"
  lightable_options <- match.arg(lightable_options,
                                 choices = c("basic", "striped", "hover"),
                                 several.ok = TRUE)
  if ("striped" %in% lightable_options) {
    light_class <- paste(light_class, "lightable-striped")
  }
  if ("hover" %in% lightable_options) {
    light_class <- paste(light_class, "lightable-hover")
  }
  kable_styling(kable_input, "none", lightable_class = light_class, ...)
}

#' @rdname kable_classic
#' @export
kable_material <- function(kable_input, lightable_options = "basic", ...) {
  light_class <- "lightable-material"
  lightable_options <- match.arg(lightable_options,
                                 choices = c("basic", "striped", "hover"),
                                 several.ok = TRUE)
  if ("striped" %in% lightable_options) {
    light_class <- paste(light_class, "lightable-striped")
  }
  if ("hover" %in% lightable_options) {
    light_class <- paste(light_class, "lightable-hover")
  }
  kable_styling(kable_input, "none", lightable_class = light_class, ...)
}
