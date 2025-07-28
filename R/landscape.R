#' Print the table on an isolated landscape page in PDF
#'
#' @description This function will put the table on an single landscape page.
#' It's useful for wide tables that can't be printed on a portrait page.
#'
#' @param kable_input Output of `knitr::kable()` with `format` specified
#' @param margin Customizable page margin for special needs. Values can be
#' "1cm", "1in" or similar.
#'
#' @examples
#' \dontrun{
#' landscape(knitr::kable(head(mtcars), "latex"))
#' }
#'
#' @export
landscape <- function(kable_input, margin = NULL) {
  kable_format <- attr(kable_input, "format")
  if (kable_format %in% c("pipe", "markdown")) {
    kable_input <- md_table_parser(kable_input)
    kable_format <- attr(kable_input, "format")
  }
  if (!kable_format %in% c("html", "latex")) {
    warning("Please specify format in kable. kableExtra can customize either ",
            "HTML or LaTeX outputs. See https://haozhu233.github.io/kableExtra/ ",
            "for details.")
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
  out <- paste0(
    "\n\\begin{landscape}",
    solve_enc(kable_input),
    "\n\\end{landscape}"
  )

  if (!is.null(margin)) {
    out <- paste0(
      "\n\\newgeometry{margin=", margin, "}", out, "\n\\restoregeometry"
    )
  }
  out <- finalize_latex(out, kable_attrs, kable_attrs$kable_meta)
  attr(out, "landscape") <- TRUE
  return(out)
}
