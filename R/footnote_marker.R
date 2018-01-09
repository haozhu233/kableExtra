#' Footnote marker
#'
#' @description Put footnote mark in superscription in table. Unless you are
#' using it in the `caption` of `kable`, you will need to put `escape = F`
#' in `kable` (similar with `cell_spec`). Again, similar with `cell_spec`, the
#' `format` option here can read default value from global option
#' `knitr.table.format`.
#'
#' @param x a number. For example, for `footnote_marker_alphabet(2)` will
#' return "b" in HTML.
#' @param format Either `html` or `latex`. All functions here can read
#' default value from global option `knitr.table.format`.
#'
#' @export
footnote_marker_number <- function(x, format) {
  if (missing(format) || is.null(format)) {
    format <- getOption('knitr.table.format')
  }
  if (is.null(format)) {
    message("Setting footnote_marker_number format as html")
    format <- "html"
  }
  if (format == "html") {
    return(paste0("<sup>", x, "</sup>"))
  } else {
    return(paste0("\\textsuperscript{", x, "}"))
  }
}

#' @rdname footnote_marker_number
#' @export
footnote_marker_alphabet <- function(x, format) {
  if (missing(format) || is.null(format)) {
    format <- getOption('knitr.table.format')
  }
  if (is.null(format)) {
    message("Setting footnote_marker_alphabet format as html")
    format <- "html"
  }
  if (is.numeric(x)) x <- letters[x]
  if (format == "html") {
    return(paste0("<sup>", x, "</sup>"))
  } else {
    return(paste0("\\textsuperscript{", x, "}"))
  }
}

#' @rdname footnote_marker_number
#' @export
footnote_marker_symbol <- function(x, format) {
  if (missing(format) || is.null(format)) {
    format <- getOption('knitr.table.format')
  }
  if (is.null(format)) {
    message("Setting footnote_marker_symbol format as html")
    format <- "html"
  }
  number_index <- read.csv(system.file("symbol_index.csv",
                                       package = "kableExtra"))
  if (format == "html") {
    x <- number_index$symbol.html[x]
    return(paste0("<sup>", x, "</sup>"))
  } else {
    x <- number_index$symbol.latex[x]
    return(paste0("\\textsuperscript{", x, "}"))
  }
}
