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
    parsed <- kable_to_parsed(kable_input)
    res <- landscape_latex(parsed, margin)
    parsed_to_kable(res, kable_input)
  }
}


landscape_latex <- function(parsed, margin) {
  kable_attrs <- attributes(parsed)
  parsed <- latex2(
    "\n", new_env("landscape",
                  parsed,
                  "\n"))

  if (!is.null(margin)) {
    parsed <- latex2(
      "\n", "\\newgeometry",
      new_block("margin=", margin), parsed,
      "\n\\restoregeometry")
  }
  attributes(parsed) <- kable_attrs
  table_info <- attr(parsed, "kable_meta")
  if (!is.null(table_info)) {
    table_info$tabularPath <- c(2, table_info$tabularPath)
    if (!is.null(table_info$tablePath))
      table_info$tablePath <- c(2, table_info$tablepath)
    parsed <- update_meta(parsed, table_info)
  }

  attr(parsed, "landscape") <- TRUE
  parsed
}
