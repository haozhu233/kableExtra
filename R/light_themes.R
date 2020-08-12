#' Alternative HTML themes
#'
#' @description kableExtra uses the built-in bootstrap themes by default in
#' `kable_styling()`. Alternatively, you can use a customized table themes for
#' your table. This `lightable` table style sheet comes with three formats,
#' namely `lightable-minimal`, `lightable-classic`, `lightable-material` and
#' `lightable-material-dark` with `hover` and `striped` options.
#'
#' @param kable_input A HTML kable object.
#' @param lightable_options Options to customize lightable. Similar with
#' `bootstrap_options` in `kable_styling`. Choices include `basic`, `striped`
#' and `hover`.
#' @param ... Everything else you need to specify in `kable_styling`.
#'
#' @export
kable_classic <- function(kable_input, lightable_options = "basic", ...) {
  kable_light(kable_input, "lightable-classic", lightable_options, ...)
}

#' @rdname kable_classic
#' @export
kable_classic_2 <- function(kable_input, lightable_options = "basic", ...) {
  kable_light(kable_input, "lightable-classic-2", lightable_options, ...)
}

#' @rdname kable_classic
#' @export
kable_minimal <- function(kable_input, lightable_options = "basic", ...) {
  kable_light(kable_input, "lightable-minimal", lightable_options, ...)
}

#' @rdname kable_classic
#' @export
kable_material <- function(kable_input, lightable_options = "basic", ...) {
  kable_light(kable_input, "lightable-material", lightable_options, ...)
}

#' @rdname kable_classic
#' @export
kable_material_dark <- function(kable_input, lightable_options = "basic", ...) {
  kable_light(kable_input, "lightable-material-dark", lightable_options, ...)
}

kable_light <- function(kable_input, light_class, lightable_options, ...) {
  lightable_options <- match.arg(lightable_options,
                                 choices = c("basic", "striped", "hover"),
                                 several.ok = TRUE)
  if ("striped" %in% lightable_options) {
    light_class <- paste(light_class, "lightable-striped")
  }
  if ("hover" %in% lightable_options) {
    light_class <- paste(light_class, "lightable-hover")
  }
  out <- kable_styling(kable_input, "none", htmltable_class = light_class, ...)
  attr(out, "lightable") <- TRUE
  attr(out, "lightable_class") <- light_class
  return(out)
}
