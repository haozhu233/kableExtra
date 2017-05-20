#' Print the table on an isolated landscape page in PDF
#'
#' @description For very wide tables in PDF documents, sometimes it might be a
#' good idea to have a single landscape page to house it. This function
#' will rotate the PDF page for the applied table in LaTeX environment. It won't
#' have any effects on HTML.
#'
#' @param kable_input Output of `knitr::kable()` with `format` specified
#' @param margin Customizable page margin for special needs. Values can be
#' "1cm", "1in" or similar.
#'
#' @export
landscape <- function(kable_input, margin = NULL) {
  kable_format <- attr(kable_input, "format")
  if (!kable_format %in% c("html", "latex")) {
    message("Currently generic markdown table using pandoc is not supported.")
    return(kable_input)
  }
  if (kable_format == "html") {
    return(kable_input)
  }
  if (kable_format == "latex") {
    return(landscape_latex(kable_input, margin))
  }
}

landscape_latex <- function(kable_input, margin) {
  kable_attrs <- attributes(kable_input)
  usepackage_latex("pdflscape")
  out <- paste0(
    "\n\\begin{landscape}", kable_input, "\n\\end{landscape}"
  )

  if (!is.null(margin)) {
    out <- paste0(
      "\n\\newgeometry{margin=", margin, "}", out, "\n\\restoregeometry"
    )
  }
  out <- structure(out, format = "latex", class = "knitr_kable")
  attributes(out) <- kable_attrs
  attr(out, "landscape") <- TRUE
  return(out)
}
