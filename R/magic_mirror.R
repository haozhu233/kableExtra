#' Magic mirror that returns kable's attributes
#'
#' @description Mirror mirror tell me, how does this kable look like?
#'
#' @param kable_input The output of kable
#'
#' @examples magic_mirror(knitr::kable(head(mtcars), "html"))
#' @import parseLatex
#' @export
magic_mirror <- function(kable_input){
  kable_format <- attr(kable_input, "format")
  if (kable_format == "latex") {
    if (inherits(kable_input, "LaTeX2"))
      parsed <- kable_input
    else
      parsed <- parseLatex(kable_input)
    table_info <- magic_mirror_latex(parsed)
  }
  if (kable_format == "html") {
    table_info <- magic_mirror_html(kable_input)
  }
  return(table_info)
}

# Magic mirror for latex tables --------------
getTabularPath <- function(parsed)
  path_to(parsed, is_fn = is_env,
          envtypes = c("tabular", "tabularx", "longtable", "tabu"))

magic_mirror_latex <- function(parsed){
  table_info <- attr(parsed, "kable_meta")
  if (!is.null(table_info))
    return(table_info)

  table_info <- list(tabular = NULL, booktabs = FALSE, align = NULL,
                     valign = NULL, ncol = NULL, nrow = NULL, colnames = NULL,
                     rownames = NULL, caption = NULL, caption.short = NULL,
                     contents = NULL,
                     centering = FALSE, table_env = FALSE,
                     tablePath = NULL,
                     tabularPath = NULL)

  tablePath <- path_to(parsed, is_fn = is_env,
                       envtypes = "table")
  if (!length(tablePath)) tablePath <- NULL
  table_info$tablePath <- tablePath
  tabularPath <- getTabularPath(parsed)
  table_info$tabularPath <- tabularPath
  table <- parsed[[tabularPath]]

  # Tabular
  table_info$tabular <- envName(table)

  # Booktabs
  table_info$booktabs <- length(path_to(table, is_fn = is_macro, "\\toprule")) > 0

  # Alignment is a sequence with each element being a single letter, or a
  # single letter followed by a measurement in braces, e.g. "p{1cm}"
  align <- get_contents(columnOptions(table))
  align <- drop_items(align, find_char(align, "|"))
  table_info$align <- align

  # valign
  table_info$valign <- deparseLatex(posOption(table))

  # N of columns
  table_info$ncol <- ncol <- tableNcol(table)

  # Caption
  table_info$captionPath <- path <- path_to_caption(parsed)
  if (length(path)) {
    table_info$caption <- get_contents(parsed[[path]])
    idx <- attr(path, "idx")
    fullcaption <- get_range(parsed, idx)
    table_info$caption.short <- bracket_options(fullcaption, start = 2)
  }

  if (!length(table_info$caption.short)) table_info["caption.short"] <- list(NULL)

  table_info$contents <- lapply(seq_len(tableNrow(table)),
                                function(i) tableRow(table, i))

  if (!is.null(attr(parsed, "n_head"))) {
    n_head <- attr(parsed, "n_head")
    table_info$new_header_row <- table_info$contents[seq(n_head - 1, 1)]
    table_info$contents <- table_info$contents[-seq(1, n_head - 1)]
    table_info$header_df <- extra_header_to_header_df(table_info$new_header_row)
    table_info$new_header_row <- paste0(table_info$new_header_row, "\\\\\\\\")
  }
  table_info$nrow <- nrow <- tableNrow(table)
  # Column names
  if (table_info$booktabs && length(find_sequence(parsed, "\\midrule")) == 0) {
    table_info$colnames <- NULL
    table_info$position_offset <- 0
  } else {
    colnames <- character(ncol)
    for (i in seq_len(ncol))
      colnames[i] <- trimws(deparseLatex(tableCell(table, 1, i)))
    table_info$colnames <- colnames
    table_info$position_offset <- 1
  }
  # Row names
  rownames <- character(nrow)
  for (i in seq_len(nrow))
    rownames[i] <- trimws(deparseLatex(tableCell(table, i, 1)))

  table_info$rownames <- rownames

  table_info$centering <- find_macro(parsed, "\\centering")

  table_info$table_env <- !is.null(table_info$tablePath)

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


