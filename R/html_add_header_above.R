#' Add an extra header row above the current header
#' @export
htmlTable_add_header_above <- function(kable_input, header = NULL) {
  # if (is.null(header)) return(kable_input)
  kable_xml <- read_xml(as.character(kable_input), options = c("COMPACT"))
  # somehow xml2 cannot directly search by name here (it will result in a crash)
  kable_xml_thead <- xml_child(kable_xml, 1)
  if (xml_name(kable_xml_thead) != "thead") {
    kable_xml_thead <- xml_child(kable_xml, 2)
  }

  header <- standardize_header_input(header)

  header_rows <- xml_children(kable_xml_thead)
  bottom_header_row <- header_rows[[length(header_rows)]]
  kable_ncol <- length(xml_children(bottom_header_row))
  if (sum(header$colspan) != kable_ncol) {
    stop("The new header row you provided has a different total number of ",
         "columns with the original kable output.")
  }

  new_header_row <- new_header_generator(header)
  xml_add_child(kable_xml_thead, new_header_row, .where = 0)
  return(structure(as.character(kable_xml), format = "html",
                   class = "knitr_kable"))
}

standardize_header_input <- function(header) {
  header_names <- names(header)

  if (is.null(header_names)) {
    return(data.frame(header = header, colspan = 1, row.names = NULL))
  }

  names(header)[header_names == ""] <- header[header_names == ""]
  header[header_names == ""] <- 1
  header_names <- names(header)
  header <- as.numeric(header)
  names(header) <- header_names
  return(data.frame(header = names(header), colspan = header, row.names = NULL))
}

new_header_generator <- function(header_vector) {
  header_items <- apply(header_vector, 1, function(x) {
    paste0('<th style="text-align:center;" colspan="', x[2], '">',
           x[1], '</th>')
  })
  header_text <- paste(c("<tr>", header_items, "</tr>"), collapse = "")
  header_xml <- read_xml(header_text, options = c("COMPACT"))
  return(header_xml)
}
