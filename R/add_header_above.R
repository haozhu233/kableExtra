#' Add an extra header row above the current header
#' @export
add_header_above <- function(kable_input, header = NULL) {
  kable_format <- attr(kable_input, "format")
  if (!kable_format %in% c("html", "latex")) {
    stop("Please specify output format in your kable function. Currently ",
         "generic markdown table using pandoc is not supported.")
  }
  if (kable_format == "html") {
    return(htmlTable_add_header_above(kable_input, header))
  }
  if (kable_format == "latex") {
    return(pdfTable_add_header_above(kable_input, header))
  }
}

# HTML
htmlTable_add_header_above <- function(kable_input, header = NULL) {
  if (is.null(header)) return(kable_input)
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

  new_header_row <- htmlTable_new_header_generator(header)
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

htmlTable_new_header_generator <- function(header_df) {
  header_items <- apply(header_df, 1, function(x) {
    paste0('<th style="text-align:center;" colspan="', x[2], '">',
           x[1], '</th>')
  })
  header_text <- paste(c("<tr>", header_items, "</tr>"), collapse = "")
  header_xml <- read_xml(header_text, options = c("COMPACT"))
  return(header_xml)
}

# Add an extra header row above the current header in a LaTeX table ------
pdfTable_add_header_above <- function(kable_input, header = NULL) {
  table_info <- magic_mirror(kable_input)
  header <- standardize_header_input(header)
  hline_type <- switch(table_info$booktabs + 1, "\\\\hline", "\\\\toprule")
  out <- sub(hline_type,
             paste0(hline_type, "\n",
                    pdfTable_new_header_generator(header, table_info$booktabs)),
             as.character(kable_input))
  out <- structure(out, format = "latex", class = "knitr_kable")
  return(out)
}

pdfTable_new_header_generator <- function(header_df, booktabs = F) {
  if (booktabs) {
    header_df$align <- "c"
  } else {
    header_df$align <- "|c|"
    header_df$align[1] <- "c|"
    header_df$align[nrow(header_df)] <- "|c"
  }
  header_items <- apply(header_df, 1, function(x) {
    # if(x[2] == 1) return(x[1])
    paste0('\\\\multicolumn{', x[2], '}{', x[3], '}{', x[1], "}")
  })
  header_text <- paste(paste(header_items, collapse = " & "), "\\\\\\\\")
  cline_end <- cumsum(header_df$colspan)
  cline_start <- c(0, cline_end) + 1
  cline_start <- cline_start[-length(cline_start)]
  cline_type <- switch(booktabs + 1, "\\\\cline{", "\\\\cmidrule(l{2pt}r{2pt}){")
  cline <- paste0(cline_type, cline_start, "-", cline_end, "}")
  cline <- cline[trimws(header_df$header) != ""]
  cline <- paste(cline, collapse = " ")
  header_text <- paste(header_text, cline)
  return(header_text)
}
