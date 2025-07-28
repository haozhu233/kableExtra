#' Magic mirror that returns kable's attributes
#'
#' @description Mirror mirror tell me, how does this kable look like?
#'
#' @param kable_input The output of kable
#'
#' @examples magic_mirror(knitr::kable(head(mtcars), "html"))
#' @export

magic_mirror <- function(kable_input){
  kable_format <- attr(kable_input, "format")
  if (kable_format == "latex") {
    table_info <- magic_mirror_latex(kable_input)
  }
  if (kable_format == "html") {
    table_info <- magic_mirror_html(kable_input)
  }
  if ("kable_meta" %in% names(attributes(kable_input))) {
    out <- attr(kable_input, "kable_meta")
    # if we return `kable_meta` immediately, `kable_styling` will use the
    # original `table_env` value. So if we call `kable_styling` twice on the
    # same object, it will nest a table within a table. Make sure this does not
    # happen.
    if (kable_format == "latex") {
      table_info <- magic_mirror_latex(kable_input)
      if (table_info$table_env && !out$table_env) {
        out$table_env <- table_info$table_env
      }
    }
    return(out)
  }
  return(table_info)
}

# Magic mirror for latex tables --------------
magic_mirror_latex <- function(kable_input){
  table_info <- list(tabular = NULL, booktabs = FALSE, align = NULL,
                     valign = NULL, ncol = NULL, nrow = NULL, colnames = NULL,
                     rownames = NULL, caption = NULL, caption.short = NULL,
                     contents = NULL,
                     centering = FALSE, table_env = FALSE)
  kbl_booktabs <- attr(kable_input, "kbl_booktabs")
  # Tabular
  table_info$tabular <- ifelse(
    grepl("\\\\begin\\{tabular\\}", kable_input),
    "tabular",
    ifelse(
      grepl("\\\\begin\\{tabularx\\}", kable_input),
      "tabularx",
      "longtable"
    )
  )

  # Booktabs
  if (!is.null(kbl_booktabs))
    table_info$booktabs <- kbl_booktabs
  else
    table_info$booktabs <- grepl(toprule_regexp, kable_input)
  # Alignment is a sequence with each element being a single letter, or a
  # single letter followed by a measurement in braces, e.g. "p{1cm}"
  align1 <- "[[:alpha:]](?:\\{[^{}]*\\})?\\|*"
  align_pattern <- paste0("(?:", align1, ")*")
  align <- str_match(kable_input,
                     paste0("\\\\begin\\{",
                        table_info$tabular,
                        "\\}[^{]*(?:\\{[^{]*\\})?\\{(",
                        align_pattern, ")\\}"))[1,2]
  table_info$align <- gsub("\\|", "", align)
  table_info$align_vector <- regmatches(table_info$align, gregexpr("[[:alpha:]](\\{[^{}]*\\})?", table_info$align))[[1]]
  table_info$align_vector_origin <- table_info$align_vector
  # valign
  table_info$valign <- gsub("\\|", "", str_match(
    kable_input, paste0("\\\\begin\\{", table_info$tabular,"\\}([^{]*)\\{.*?\\}"))[2])
  table_info$valign2 <- sub("\\[", "\\\\[", table_info$valign)
  table_info$valign2 <- sub("\\]", "\\\\]", table_info$valign2)
  table_info$valign3 <- sub("\\[", "", table_info$valign)
  table_info$valign3 <- sub("\\]", "", table_info$valign3)
  table_info$begin_tabular <- paste0("\\\\begin\\{", table_info$tabular, "\\}",
                                     table_info$valign2)
  table_info$end_tabular <- paste0("\\\\end\\{", table_info$tabular, "\\}")
  # N of columns
  table_info$ncol <- length(table_info$align_vector)
  # Caption
  if (str_detect(kable_input, "caption\\[")) {
    caption_line <- str_match(kable_input, "\\\\caption(.*)\\n")[2]
    table_info$caption.short <- str_match(caption_line, "\\[(.*?)\\]")[2]
    table_info$caption <- substr(caption_line,
                                 nchar(table_info$caption.short) + 4,
                                 nchar(caption_line))
  } else {
    table_info$caption <- str_match(kable_input, "caption\\{(.*?)\\n")[2]
  }
  if (table_info$tabular == "longtable") {
    table_info$caption <- str_sub(table_info$caption, 1, -4)
  } else {
    table_info$caption <- str_sub(table_info$caption, 1, -2)
  }
  # Contents
  table_info$contents <- str_match_all(kable_input, "\n(.*)\\\\\\\\")[[1]][,2]
  table_info$contents <- regex_escape(table_info$contents, T)
  if (table_info$tabular == "longtable" & !is.na(table_info$caption) &
      !str_detect(kable_input, "\\\\begin\\{table\\}\\n\\n\\\\caption")) {
    table_info$contents <- table_info$contents[-1]
  }
  if (!is.null(attr(kable_input, "n_head"))) {
    n_head <- attr(kable_input, "n_head")
    table_info$new_header_row <- table_info$contents[seq(n_head - 1, 1)]
    table_info$contents <- table_info$contents[-seq(1, n_head - 1)]
    table_info$header_df <- extra_header_to_header_df(table_info$new_header_row)
    table_info$new_header_row <- paste0(table_info$new_header_row, "\\\\\\\\")
  }
  table_info$nrow <- length(table_info$contents)
  table_info$duplicated_rows <- (sum(duplicated(table_info$contents)) != 0)
  # Column names
  if (table_info$booktabs & !grepl(midrule_regexp, kable_input)) {
    table_info$colnames <- NULL
    table_info$position_offset <- 0
  } else {
    table_info$colnames <- str_split(table_info$contents[1], " \\& ")[[1]]
    table_info$position_offset <- 1
  }
  # Row names
  table_info$rownames <- str_extract(table_info$contents, "^[^ &]*")

  table_info$centering <- grepl("\\\\centering", kable_input)

  table_info$table_env <- (!is.na(table_info$caption) &
                           table_info$tabular != "longtable") ||
                          grepl("\\\\begin\\{table\\}", kable_input)

  return(table_info)
}

extra_header_to_header_df <- function(extra_header_rows) {
  lapply(str_split(extra_header_rows, " \\& "), function(x) {
    as.data.frame(t(sapply(x, extra_header_to_header_df_)), row.names = NA)
  })
}

extra_header_to_header_df_ <- function(x) {
  if (trimws(x) == "") return(c(header = " ", colspan = "1"))
  x <- trimws(x)
  x_header <- str_match(x, "([^\\}\\{]*)\\\\\\}$")[2]
  x_colspan <- str_match(x, "^\\\\\\\\multicolumn\\\\\\{([^\\\\\\}]*)")[2]
  return(c(header = x_header, colspan = x_colspan))
}

# Magic Mirror for html table --------
magic_mirror_html <- function(kable_input){
  table_info <- list()
  important_nodes <- read_kable_as_xml(kable_input)
  body_node <- important_nodes$body
  kable_xml <- important_nodes$table
  # Caption
  table_info$caption <- xml_text(xml_child(kable_xml, "caption"))
  # colnames
  table_info$colnames <- lapply(xml_children(xml_child(kable_xml, "thead")),
                                xml_children)
  table_info$colnames <- table_info$colnames[[length(table_info$colnames)]]
  table_info$colnames <- trimws(xml_text(table_info$colnames))
  table_info$ncol <- length(table_info$colnames)
  table_info$nrow_header <- length(xml_children(xml_child(kable_xml, "thead")))
  table_info$nrow_body <- nrow(table_info$contents)
  table_info$table_class <- xml_attr(kable_xml, "class")
  table_info$table_style <- xml_attr(kable_xml, "style")
  return(table_info)
}


