#' HTML table attributes
#'
#' @description This function provides a set of shortcuts to common HTML table
#' formats
#'
#' @param bootstrap_options Common HTML table formats
#' @param font_size
#'
#' @export
htmlTable_styling <- function(bootstrap_options = "bs-table",
                             font_size = NULL, full_width = T) {
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
  if (length(table_attr_style) != 0) {
    table_attr_style <- paste0("style = '",
                               paste0(table_attr_style, collapse = " "), "'")
  }
  return(paste(table_attr_class, table_attr_style))
}
