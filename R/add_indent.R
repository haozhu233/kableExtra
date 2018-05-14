#' Add indentations to row headers
#'
#' @param kable_input Output of `knitr::kable()` with `format` specified
#' @param positions A vector of numeric row numbers for the rows that need to
#' be indented.
#'
#' @examples x <- knitr::kable(head(mtcars), "html")
#' # Add indentations to the 2nd & 4th row
#' add_indent(x, c(2, 4))
#'
#' @export
add_indent <- function(kable_input, positions) {
  if (!is.numeric(positions)) {
    stop("Positions can only take numeric row numbers (excluding header rows).")
  }
  kable_format <- attr(kable_input, "format")
  if (!kable_format %in% c("html", "latex")) {
    warning("Please specify format in kable. kableExtra can customize either ",
            "HTML or LaTeX outputs. See https://haozhu233.github.io/kableExtra/ ",
            "for details.")
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
  table_info <- magic_mirror(kable_input)
  out <- solve_enc(kable_input)

  if (table_info$duplicated_rows) {
    dup_fx_out <- fix_duplicated_rows_latex(out, table_info)
    out <- dup_fx_out[[1]]
    table_info <- dup_fx_out[[2]]
  }

  max_position <- table_info$nrow - table_info$position_offset

  if (max(positions) > max_position) {
    stop("There aren't that many rows in the table. Check positions in ",
         "add_indent_latex.")
  }

  for (i in positions + table_info$position_offset) {
    rowtext <- table_info$contents[i]
    out <- sub(rowtext, latex_indent_unit(rowtext), out, perl = TRUE)
    table_info$contents[i] <- latex_indent_unit(rowtext)
  }
  out <- structure(out, format = "latex", class = "knitr_kable")
  attr(out, "kable_meta") <- table_info
  return(out)
}

latex_indent_unit <- function(rowtext) {
  paste0("\\\\hspace\\{1em\\}", rowtext)
}

# Add indentation for HTML
add_indent_html <- function(kable_input, positions) {
  kable_attrs <- attributes(kable_input)

  kable_xml <- read_kable_as_xml(kable_input)
  kable_tbody <- xml_tpart(kable_xml, "tbody")

  group_header_rows <- attr(kable_input, "group_header_rows")
  if (!is.null(group_header_rows)) {
    positions <- positions_corrector(positions, group_header_rows,
                                     length(xml_children(kable_tbody)))
  }

  for (i in positions) {
    node_to_edit <- xml_child(xml_children(kable_tbody)[[i]], 1)
    if (!xml_has_attr(node_to_edit, "indentlevel")) {
      xml_attr(node_to_edit, "style") <- paste(
        xml_attr(node_to_edit, "style"), "padding-left: 2em;"
      )
      xml_attr(node_to_edit, "indentlevel") <- 1
    } else {
      indentLevel <- as.numeric(xml_attr(node_to_edit, "indentlevel"))
      xml_attr(node_to_edit, "style") <- sub(
        paste0("padding-left: ", indentLevel * 2, "em;"),
        paste0("padding-left: ", (indentLevel + 1) * 2, "em;"),
        xml_attr(node_to_edit, "style")
      )
      xml_attr(node_to_edit, "indentlevel") <- indentLevel + 1
    }
  }
  out <- as_kable_xml(kable_xml)
  attributes(out) <- kable_attrs
  if (!"kableExtra" %in% class(out)) class(out) <- c("kableExtra", class(out))
  return(out)
}
