#' Add indentations to row headers
#' @export
add_indent <- function(kable_input, positions) {
  kable_format <- attr(kable_input, "format")
  if (!kable_format %in% c("html", "latex")) {
    message("Currently generic markdown table using pandoc is not supported.")
    return(kable_input)
  }
  if (kable_format == "html") {
    return(add_indent_html(kable_input, positions))
  }
  if (kable_format == "latex") {
    return(add_indent_latex(kable_input, positions))
  }
}

# Add indentation for LaTeX
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

# Add indentation for HTML
add_indent_html <- function(kable_input, positions) {
  kable_attrs <- attributes(kable_input)

  kable_xml <- read_xml(as.character(kable_input), options = "COMPACT")
  kable_tbody <- xml_tpart(kable_xml, "tbody")

  group_header_rows <- attr(kable_input, "group_header_rows")
  if (!is.null(group_header_rows)) {
    positions <- positions_corrector(positions, group_header_rows,
                                     length(xml_children(kable_tbody)))
  }
  for (i in positions) {
    node_to_edit <- xml_child(xml_children(kable_tbody)[[i]], 1)
    if (!xml_has_attr(node_to_edit, "indentLevel")) {
      xml_attr(node_to_edit, "style") <- paste(
        xml_attr(node_to_edit, "style"), "padding-left: 2em;"
      )
      xml_attr(node_to_edit, "indentLevel") <- 1
    } else {
      indentLevel <- as.numeric(xml_attr(node_to_edit, "indentLevel"))
      xml_attr(node_to_edit, "style") <- sub(
        paste0("padding-left: ", indentLevel * 2, "em;"),
        paste0("padding-left: ", (indentLevel + 1) * 2, "em;"),
        xml_attr(node_to_edit, "style")
      )
      xml_attr(node_to_edit, "indentLevel") <- indentLevel + 1
    }
  }
  out <- structure(as.character(kable_xml), format = "html",
                   class = "knitr_kable")
  attributes(out) <- kable_attrs
  return(out)
}
