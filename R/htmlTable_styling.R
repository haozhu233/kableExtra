#' HTML table attributes
#'
#' @description This function provides a set of shortcuts to common HTML table
#' formats
#'
#' @param bootstrap_options A character vector for bootstrap table options. For
#' detailed information, please check the package vignette or visit the
#' w3schools' [Bootstrap Page](https://www.w3schools.com/bootstrap/bootstrap_tables.asp)
#' . Possible options include "bs-table", "striped", "bordered", "hover",
#' "condensed" and "responsive".
#' @param full_width A `TRUE` of `FALSE` variable controlling whether the HTML
#' table should have 100\% width.
#' @param float A character string determining whether and how the HTML table
#' should float on the page.
#' @param font_size A numeric input for table font size
#'
#' @export
htmlTable_styling <- function(bootstrap_options = "bs-table",
                             full_width = T,
                             float = c("left", "center", "right"),
                             font_size = NULL) {
  bootstrap_options <- match.arg(
    bootstrap_options,
    c("bs-table", "striped", "bordered", "hover", "condensed", "responsive"),
    several.ok = T
  )

  table_attr_class <- character()
  if (length(bootstrap_options) == 1 && bootstrap_options == "bs-table") {
    table_attr_class <- "class='table'"
  } else {
    bootstrap_options <- bootstrap_options[bootstrap_options != "bs-table"]
    bootstrap_options <- paste0("table-", bootstrap_options)
    table_attr_class <- paste0("class='table ",
                               paste0(bootstrap_options, collapse = " "), "'")
  }

  table_attr_style <- c()
  if (!is.null(font_size)) {
    table_attr_style <- c(table_attr_style,
                          paste0("font-size: ", font_size, "px;"))
  }
  if (!full_width) {
    table_attr_style <- c(table_attr_style, "width: auto !important;")
  }

  float <- match.arg(float)
  if (float == "center") {
    table_attr_style <- c(table_attr_style,
                          "margin-left:auto; margin-right:auto;")
  }
  if (float == "right") {
    table_attr_style <- c(table_attr_style,
                          "float: right;")
  }
  if (length(table_attr_style) != 0) {
    table_attr_style <- paste0("style = '",
                               paste0(table_attr_style, collapse = " "), "'")
  }
  return(paste(table_attr_class, table_attr_style))
}
