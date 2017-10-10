#' Add a header row on top of current header
#'
#' @description Tables with multiple rows of header rows are extremely useful
#' to demonstrate grouped data. This function takes the output of a `kable()`
#' function and adds an header row on top of it.
#'
#' @param kable_input Output of `knitr::kable()` with `format` specified
#' @param header A (named) character vector with `colspan` as values. For
#' example, `c(" " = 1, "title" = 2)` can be used to create a new header row
#' for a 3-column table with "title" spanning across column 2 and 3. For
#' convenience, when `colspan` equals to 1, users can drop the ` = 1` part.
#' As a result, `c(" ", "title" = 2)` is the same as `c(" " = 1, "title" = 2)`.
#' @param bold A T/F value to control whether the text should be bolded.
#' @param italic A T/F value to control whether the text should to be emphasized.
#' @param monospace A T/F value to control whether the text of the selected column
#' need to be monospaced (verbatim)
#' @param escape A T/F value showing whether special characters should be
#' escaped.
#'
#' @examples x <- knitr::kable(head(mtcars), "html")
#' # Add a row of header with 3 columns on the top of the table. The column
#' # span for the 2nd and 3rd one are 5 & 6.
#' add_header_above(x, c(" ", "Group 1" = 5, "Group 2" = 6))
#'
#' @export
add_header_above <- function(kable_input, header = NULL,
                             bold = FALSE, italic = FALSE,
                             monospace = FALSE, escape = TRUE) {
  kable_format <- attr(kable_input, "format")
  if (!kable_format %in% c("html", "latex")) {
    stop("Please specify output format in your kable function. Currently ",
         "generic markdown table using pandoc is not supported.")
  }
  if (kable_format == "html") {
    return(htmlTable_add_header_above(kable_input, header,
                                      bold, italic, monospace, escape))
  }
  if (kable_format == "latex") {
    return(pdfTable_add_header_above(kable_input, header,
                                     bold, italic, monospace, escape))
  }
}

# HTML
htmlTable_add_header_above <- function(kable_input, header,
                                       bold, italic, monospace, escape) {
  if (is.null(header)) return(kable_input)
  kable_attrs <- attributes(kable_input)
  kable_xml <- read_kable_as_xml(kable_input)
  kable_xml_thead <- xml_tpart(kable_xml, "thead")

  header <- standardize_header_input(header)

  if (escape) {
    header$header <- escape_html(header$header)
  }

  header_rows <- xml_children(kable_xml_thead)
  bottom_header_row <- header_rows[[length(header_rows)]]
  kable_ncol <- length(xml_children(bottom_header_row))
  if (sum(header$colspan) != kable_ncol) {
    stop("The new header row you provided has a different total number of ",
         "columns with the original kable output.")
  }

  new_header_row <- htmlTable_new_header_generator(header,
                                                   bold, italic, monospace)
  xml_add_child(kable_xml_thead, new_header_row, .where = 0)
  out <- as_kable_xml(kable_xml)
  attributes(out) <- kable_attrs
  return(out)
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

htmlTable_new_header_generator <- function(header_df, bold, italic, monospace) {
  row_style <- paste0(
    ifelse(bold, "font-weight: bold; ", ""),
    ifelse(italic, "font-style: italic; ", ""),
    ifelse(monospace, "font-family: monospace; ", "")
  )
  header_items <- apply(header_df, 1, function(x) {
    if (trimws(x[1]) == "") {
      paste0('<th style="border-bottom:hidden"></th>')
    } else {
      paste0('<th style="text-align:center; border-bottom:hidden; ',
             'padding-bottom:0; padding-left:3px;padding-right:3px;',
             row_style,
             '" colspan="',
             x[2], '"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px;">',
             x[1], '</div></th>')
    }
  })
  header_text <- paste(c("<tr>", header_items, "</tr>"), collapse = "")
  header_xml <- read_xml(header_text, options = c("COMPACT"))
  return(header_xml)
}

# Add an extra header row above the current header in a LaTeX table ------
pdfTable_add_header_above <- function(kable_input, header,
                                      bold, italic, monospace, escape) {
  table_info <- magic_mirror(kable_input)
  header <- standardize_header_input(header)
  if (escape) {
    header$header <- escape_latex(header$header)
    header$header <- gsub("\\\\", "\\\\\\\\", header$header)
  }
  hline_type <- switch(table_info$booktabs + 1, "\\\\hline", "\\\\toprule")
  new_header_split <- pdfTable_new_header_generator(header, table_info$booktabs,
                                                    bold, italic, monospace)
  new_header <- paste0(new_header_split[1], "\n", new_header_split[2])
  out <- str_replace(enc2utf8(as.character(kable_input)),
                     hline_type,
                     paste0(hline_type, "\n", new_header))
  out <- structure(out, format = "latex", class = "knitr_kable")
  # new_header_row <- latex_contents_escape(new_header_split[1])
  if (is.null(table_info$new_header_row)) {
    table_info$new_header_row <- new_header_split[1]
    table_info$header_df <- list(header)
  } else {
    table_info$new_header_row <- c(table_info$new_header_row, new_header_split[1])
    table_info$header_df[[length(table_info$header_df) + 1]] <- header
  }
  attr(out, "kable_meta") <- table_info
  return(out)
}

pdfTable_new_header_generator <- function(header_df, booktabs = FALSE,
                                          bold, italic, monospace) {
  if (booktabs) {
    header_df$align <- "c"
  } else {
    header_df$align <- "|c|"
    header_df$align[1] <- "c|"
    header_df$align[nrow(header_df)] <- "|c"
  }
  header_items <- apply(header_df, 1, function(x) {
    # if(x[2] == 1) return(x[1])
    paste0('\\\\multicolumn{', x[2], '}{', x[3], '}{',
           ifelse(bold, "\\\\bfseries ", ""),
           ifelse(italic, "\\\\em ", ""),
           ifelse(monospace, "\\\\ttfamily ", ""),
           x[1],
           "}")
  })
  header_text <- paste(paste(header_items, collapse = " & "), "\\\\\\\\")
  cline <- cline_gen(header_df, booktabs)
  return(c(header_text, cline))
}

cline_gen <- function(header_df, booktabs) {
  cline_end <- cumsum(header_df$colspan)
  cline_start <- c(0, cline_end) + 1
  cline_start <- cline_start[-length(cline_start)]
  cline_type <- switch(booktabs + 1,
                       "\\\\cline{",
                       "\\\\cmidrule(l{2pt}r{2pt}){")
  cline <- paste0(cline_type, cline_start, "-", cline_end, "}")
  cline <- cline[trimws(header_df$header) != ""]
  cline <- paste(cline, collapse = " ")
  return(cline)
}


