#' Convert xtable to a kable object
#'
#' @description This function allow users to turn an xtable object into a kable
#' so they can use most of kableExtra's functions with their xtable code without
#' making too many changes. Note that although I tested many cases and it seems
#' to work, this function may not be functional in some other cases. I'm not
#' a regular xtable user and can only provide very limited support for this
#' function.
#'
#' You should use this table in the same way as `print.xtable`. All the options
#' you provided to this function will be sent to `print.xtable`. Instead of
#' printing out the result, this function will return the LaTeX or HTML as
#' text and a kable object.
#'
#' @param x an xtable object
#' @param ... options for print.xtable
#'
#' @examples
#' \dontrun{
#' library(xtable)
#' xtable(mtcars) %>%
#'   xtable2kable(booktabs = TRUE) %>%
#'   kable_styling(latex_options = "striped")
#' }
#'
#' @export
xtable2kable <- function(x, ...) {
  if (!inherits(x, "xtable")) {
    warning("x is not an xtable object.")
    return(x)
  }

  out <- capture.output(print(x, ...))[-(1:2)]
  out <- paste(out, collapse = "\n")

  xtable_print_options <- list(...)
  if ("type" %in% names(xtable_print_options) &&
      xtable_print_options$type == "html") {
    out <- structure(out, format = "html", class = "knitr_kable")
    return(out)
  }

  out <- structure(out, format = "latex", class = "knitr_kable")

  # Assign modefied meta to output
  out_meta <- magic_mirror(out)

  if ("table.placement" %in% names(xtable_print_options)) {
    out_meta$valign <- paste0("[", xtable_print_options$table.placement, "]")
  }
  if ("tabular.environment" %in% names(xtable_print_options)) {
    out_meta$tabular <- xtable_print_options$tabular.environment
  }
  out_meta$xtable <- TRUE
  attr(out, "kable_meta") <- out_meta
  return(out)
}
