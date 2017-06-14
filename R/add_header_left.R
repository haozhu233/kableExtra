#' Add a header column
#'
#' @description Experimenting. Please don't use it in production
#'
#' @export
add_header_left <- function(kable_input, header = NULL, header_name = "") {
  if (is.null(header)) return(kable_input)
  kable_format <- attr(kable_input, "format")
  if (!kable_format %in% c("html", "latex")) {
    stop("Please specify output format in your kable function. Currently ",
         "generic markdown table using pandoc is not supported.")
  }
  if (kable_format == "html") {
    return(add_header_left_html(kable_input, header, header_name))
  }
  if (kable_format == "latex") {
    return(add_header_left_latex(kable_input, header, header_name))
  }
}

# HTML
add_header_left_html <- function(kable_input, header, header_name) {
  kable_attrs <- attributes(kable_input)
  kable_xml <- read_xml(as.character(kable_input), options = "COMPACT")
  kable_thead <- xml_tpart(kable_xml, "thead")
  kable_tbody <- xml_tpart(kable_xml, "tbody")

  new_header <- paste0(
    '<th style="text-align:center;" rowspan="',
    length(xml_children(kable_thead)), '">', header_name, '</th>'
  )
  new_header <- read_xml(new_header, options = c("COMPACT"))
  xml_add_child(xml_child(kable_thead, 1), new_header, .where = 0)

  header <- standardize_rowheader_input(header, length(xml_children(kable_tbody)))
  for (i in 1:nrow(header)) {
    new_row_item <- paste0(
      '<td style="text-align:center; vertical-align: middle;" rowspan="',
      header$rowspan[i], '">', header$header[i], '</td>')
    new_row_item <- read_xml(new_row_item, options = "COMPACT")
    target_row <- xml_child(kable_tbody, header$row[i])
    xml_add_child(target_row, new_row_item, .where = 0)
  }

  out <- structure(as.character(kable_xml), format = "html",
                   class = "knitr_kable")
  attributes(out) <- kable_attrs
  return(out)
}

standardize_rowheader_input <- function(header, n_row) {
  header_names <- names(header)

  if (is.null(header_names)) {
    return(data.frame(header = header, row = 1:length(header),
                      rowspan = 1, row.names = NULL))
  }

  names(header)[header_names == ""] <- header[header_names == ""]
  header[header_names == ""] <- 1
  header_names <- names(header)
  header <- as.numeric(header)
  names(header) <- header_names
  if (sum(header) < n_row) {
    header <- c(header, " " = n_row - sum(header))
  }
  row_pos <- c(1, cumsum(header)[-length(header)] + 1)
  return(data.frame(
    header = names(header),
    row = row_pos, rowspan = header, row.names = NULL
    ))
}

add_header_left_latex <- function(kable_input, header, header_name) {
  return(kable_input)
}
