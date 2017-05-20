#' Add indentations to row headers
#' @export
add_indent <- function(kable_input, positions) {
  kable_format <- attr(kable_input, "format")
  if (!kable_format %in% c("html", "latex")) {
    message("Currently generic markdown table using pandoc is not supported.")
    return(kable_input)
  }
  if (kable_format == "html") {
    return(kable_input)
  }
  if (kable_format == "latex") {
    return(add_indent_latex(kable_input, positions))
  }
}

add_indent_latex <- function(kable_input, positions) {
  table_info <- attr(kable_input, "original_kable_meta")
  if (is.null(table_info)) {
    table_info <- magic_mirror(kable_input)
  }

  if (!is.numeric(positions)) {
    stop("Positions can only take numeric row numbers (excluding header rows).")
  }
  if (max(positions) > table_info$nrow - 1) {
    stop("There aren't that many rows in the table. Check positions in ",
         "add_indent_latex.")
  }

  out <- kable_input
  for (i in positions) {
    rowtext <- table_info$contents[i + 1]
    out <- sub(rowtext, latex_indent_unit(rowtext), out)
  }
  return(out)
}

latex_indent_unit <- function(rowtext) {
  paste0("\\\\hspace{1em}", rowtext)
}
