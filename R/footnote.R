#' Add advanced footnote
#'
#' @export
add_footnote_adv <- function(kable_input,
                             general = NULL,
                             number = NULL,
                             alphabet = NULL,
                             symbol = NULL,
                             footnote_order = c("general", "number",
                                                "alphabet", "symbol"),
                             footnote_as_chunk = FALSE,
                             general_title = "Note: ",
                             number_title = "",
                             alphabet_title = "",
                             symbol_title = ""
) {
  kable_format <- attr(kable_input, "format")
  if (!kable_format %in% c("html", "latex")) {
    message("Currently generic markdown table using pandoc is not supported.")
    return(kable_input)
  }
  footnote_titles <- list(
    general = general_title, number = number_title,
    alphabet = alphabet_title, symbol = symbol_title
  )
  footnote_contents <- list(
    general = general, number = number, alphabet = alphabet, symbol = symbol
  )
  notnull <- names(footnote_contents)[!sapply(footnote_contents, is.null)]
  if (length(notnull) == 0) {return(kable_input)}
  if (length(alphabet) > 26) {
    alphabet <- alphabet[1:26]
    warning("Please don't use more than 26 footnotes in table_footnote ",
            "alphabet. Use number instead.")
  }
  if (length(symbol) > 20) {
    symbol <- symbol[1:20]
    warning("Please don't use more than 20 footnotes in table_footnote ",
            "symbol. Use number instead.")
  }
  footnote_order <- footnote_order[footnote_order %in% notnull]
  footnote_titles <- footnote_titles[footnote_order]
  footnote_contents <- footnote_contents[footnote_order]
  footnote_table <- footnote_table_maker(
    kable_format, footnote_titles, footnote_contents
  )
  if (kable_format == "html") {
    return(add_footnote_adv_html(kable_input, footnote_table, footnote_as_chunk))
  }
  # if (kable_format == "latex") {
  #   return(add_footnote_adv_latex(kable_input, footnote_table))
  # }
}

footnote_table_maker <- function(format, footnote_titles, footnote_contents) {
  number_index <- read.csv(system.file("symbol_index.csv",
                                       package = "kableExtra"))
  if (format == "latex") {
    symbol_index <- number_index$symbol.latex
  } else {
    symbol_index <- number_index$symbol.html
  }
  contents_length <- sapply(footnote_contents, length)

  if (!is.null(footnote_contents$general)) {
    footnote_contents$general <- data.frame(
      index = "",
      footnote = paste(footnote_contents$general, collapse = " ")
    )
  }
  if (!is.null(footnote_contents$number)) {
    footnote_contents$number <- data.frame(
      index = as.character(1:length(footnote_contents$number)),
      footnote = footnote_contents$number
    )
  }
  if (!is.null(footnote_contents$alphabet)) {
    footnote_contents$alphabet <- data.frame(
      index = letters[1:length(footnote_contents$alphabet)],
      footnote = footnote_contents$alphabet
    )
  }
  if (!is.null(footnote_contents$symbol)) {
    footnote_contents$symbol <- data.frame(
      index = symbol_index[1:length(footnote_contents$symbol)],
      footnote = footnote_contents$symbol
    )
  }

  out <- list()
  out$contents <- footnote_contents
  out$titles <- footnote_titles
  return(out)
}

# HTML
add_footnote_adv_html <- function(kable_input, footnote_table,
                                  footnote_as_chunk) {
  kable_attrs <- attributes(kable_input)
  kable_xml <- read_kable_as_xml(kable_input)

  new_html_footnote <- adv_html_tfoot_maker(footnote_table, footnote_as_chunk)
  xml_add_child(kable_xml, new_html_footnote)

  out <- as_kable_xml(kable_xml)
  attributes(out) <- kable_attrs
  return(out)
}

adv_html_tfoot_maker <- function(footnote_table, footnote_as_chunk) {
  footnote_types <- names(footnote_table$contents)
  footnote_text <- c()
  for (i in footnote_types) {
    footnote_text <- c(footnote_text, adv_html_tfoot_maker_(
      footnote_table$contents[[i]], footnote_table$titles[[i]], i,
      footnote_as_chunk))
  }
  footnote_text <- paste0(
    "<tfoot>", paste0(footnote_text, collapse = ""), "</tfoot>"
  )
  footnote_node <- read_html(footnote_text, options = c("RECOVER", "NOERROR"))
  return(xml_child(xml_child(footnote_node, 1), 1))
}

adv_html_tfoot_maker_ <- function(ft_contents, ft_title, ft_type, ft_chunk) {

  footnote_text <- apply(ft_contents, 1, function(x) {
    paste0('<sup>', x[1], '</sup> ', x[2])
  })
  if (ft_title != "") {
    title_text <- paste0('<strong>', ft_title, '</strong>')
    footnote_text <- c(title_text, footnote_text)
  }
  if (!ft_chunk) {
    footnote_text <- paste0(
      '<tr><td style="padding: 0; border: 0;" colspan="100%">',
      footnote_text, '</td></tr>'
    )
  } else {
    footnote_text <- paste0(
      '<tr><td style="padding: 0; border: 0;" colspan="100%">',
      paste0(footnote_text, collapse = ""),
      '</td></tr>'
    )
  }
  # }
  return(footnote_text)
}
