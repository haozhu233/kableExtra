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
#' @param double_escape T/F if output is in LaTeX, whether it should be double
#' escaped. If you are using footnote_marker in `group_rows`` labeling row or
#' `add_header_above`, you need to set this to be `TRUE`.
#'
#' @examples
#' \dontrun{
#' dt <- mtcars[1:5, 1:5]
#' colnames(dt)[1] <- paste0("mpg",
#'                           footnote_marker_alphabet(2))
#' rownames(dt)[2] <- paste0(rownames(dt)[2],
#'                           footnote_marker_alphabet(1))
#' dt[1,2] <- paste0(dt[1,2], footnote_marker_alphabet(3))
#'
#' kbl(dt, escape = FALSE) |>
#'   footnote(alphabet = c("Note a", "Note b", "Note c"))
#' }
#'
#' @export
footnote_marker_number <- function(x, format, double_escape = FALSE) {
  if (missing(format) || is.null(format)) {
    if (knitr::is_latex_output()) {
      format <- "latex"
    } else {
      format <- "html"
    }
  }
  if (format == "html") {
    return(paste0("<sup>", x, "</sup>"))
  } else if (!double_escape) {
    return(paste0("\\textsuperscript{", x, "}"))
  } else {
    return(paste0("\\\\textsuperscript{", x, "}"))
  }
}

#' @rdname footnote_marker_number
#' @export
footnote_marker_alphabet <- function(x, format, double_escape = FALSE) {
  if (missing(format) || is.null(format)) {
    if (knitr::is_latex_output()) {
      format <- "latex"
    } else {
      format <- "html"
    }
  }
  if (is.numeric(x)) x <- letters[x]
  if (format == "html") {
    return(paste0("<sup>", x, "</sup>"))
  } else if (!double_escape) {
    return(paste0("\\textsuperscript{", x, "}"))
  } else {
    return(paste0("\\\\textsuperscript{", x, "}"))
  }
}

#' @rdname footnote_marker_number
#' @export
footnote_marker_symbol <- function(x, format, double_escape = FALSE) {
  if (missing(format) || is.null(format)) {
    if (knitr::is_latex_output()) {
      format <- "latex"
    } else {
      format <- "html"
    }
  }
  number_index <- read.csv(system.file("symbol_index.csv",
                                       package = "kableExtra"))
  if (format == "html") {
    x <- number_index$symbol.html[x]
    return(paste0("<sup>", x, "</sup>"))
  } else if (!double_escape) {
    x <- number_index$symbol.latex[x]
    x <- gsub("\\\\\\\\", "\\\\", x)
    return(paste0("\\textsuperscript{", x, "}"))
  } else {
    x <- number_index$symbol.latex[x]
    return(paste0("\\\\textsuperscript{", x, "}"))
  }
}
