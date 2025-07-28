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
#' Alternatively, a data frame with two columns can be provided: The first
#' column should contain the header names (character vector) and the second
#' column should contain the colspan (numeric vector). This input can be used
#' if there are problems with Unicode characters in the headers.
#' @param bold A T/F value to control whether the text should be bolded.
#' @param italic A T/F value to control whether the text should to be emphasized.
#' @param monospace A T/F value to control whether the text of the selected column
#' need to be monospaced (verbatim)
#' @param underline A T/F value to control whether the text of the selected row
#' need to be underlined
#' @param strikeout A T/F value to control whether the text of the selected row
#' need to be struck out.
#' @param align A character string for cell alignment. For HTML, possible values could
#' be `l`, `c`, `r` plus `left`, `center`, `right`, `justify`, `initial` and `inherit`.  All
#' legal values should be accepted for LaTeX, e.g.
#' `l`, `c`, `r`, `p{2cm}`, but the more exotic ones
#' may cause problems.  If so, please post a bug
#' report!
#' @param color A character string/vector for text color. Here please pay
#' attention to the differences in color codes between HTML and LaTeX.
#' @param background A character string/vector for background color. Here please
#' pay attention to the differences in color codes between HTML and LaTeX. Also
#' note that in HTML, background defined in cell_spec won't cover the whole
#' cell.
#' @param font_size A numeric input/vector for font size. For HTML, you can also use
#' options including `xx-small`, `x-small`, `small`, `medium`, `large`,
#' `x-large`, `xx-large`, `smaller`, `larger`, `initial` and `inherit`.
#' @param angle 0-360, degree that the text will rotate.
#' @param escape A T/F value showing whether special characters should be
#' escaped.
#' @param line A T/F value/vector to control whether a line will appear underneath the
#' header
#' @param line_sep A numeric value indicating how much the midlines should be
#' separated by space. Default is 3.
#' @param extra_css An HTML only option. CSS defined here will be send to the
#' td cell.
#' @param include_empty Whether empty cells in HTML should also be styled.
#' Default is FALSE.
#' @param border_left T/F option for border on the left side in latex.
#' @param border_right T/F option for border on the right side in latex.
#'
#' @examples
#' \dontrun{
#' x <- kbl(head(mtcars), "html")
#' # Add a row of header with 3 columns on the top of the table. The column
#' # span for the 2nd and 3rd one are 5 & 6.
#' add_header_above(x, c(" ", "Group 1" = 5, "Group 2" = 6))
#' }
#'
#' @export
add_header_above <- function(kable_input, header = NULL,
                             bold = FALSE, italic = FALSE, monospace = FALSE,
                             underline = FALSE, strikeout = FALSE,
                             align = "c", color = NULL, background = NULL,
                             font_size = NULL, angle = NULL,
                             escape = TRUE, line = TRUE, line_sep = 3,
                             extra_css = NULL, include_empty = FALSE,
                             border_left = FALSE, border_right = FALSE) {
  if (is.null(header)) return(kable_input)

  kable_format <- attr(kable_input, "format")
  if (kable_format %in% c("pipe", "markdown")) {
    kable_input <- md_table_parser(kable_input)
    kable_format <- attr(kable_input, "format")
  }
  if (!kable_format %in% c("html", "latex")) {
    warning("Please specify format in kable. kableExtra can customize either ",
            "HTML or LaTeX outputs. See https://haozhu233.github.io/kableExtra/ ",
            "for details.")
    return(kable_input)
  }
  if ((length(align) != 1L) & (length(align) != length(header))) {
    warning("Length of align vector supplied to add_header_above must either be 1 ",
            "or the same length as the header supplied. The length of the align ",
            sprintf("vector supplied is %i and the header length is %i.",
                    length(align), length(header)),
            "Using default of centering each element of row.")
    align <- 'c'
  }
  if (is.null(header)) return(kable_input)
  if (is.data.frame(header)){
    if(ncol(header) == 2 & is.character(header[[1]]) & is.numeric(header[[2]])){
      header <- data.frame(header = header[[1]], colspan = header[[2]],
                           stringsAsFactors = FALSE)
    }
    else {
      stop("If header input is provided as a data frame instead of a named",
           "vector it must consist of only two columns: ",
           "The first should be a character vector with ",
           "header names and the second should be a numeric vector with ",
           "the number of columns the header should span.")
    }
  }
  else {
    header <- standardize_header_input(header)
  }
  if (kable_format == "html") {
    return(htmlTable_add_header_above(
      kable_input, header, bold, italic, monospace, underline, strikeout,
      align, color, background, font_size, angle, escape, line, line_sep,
      extra_css, include_empty
    ))
  }
  if (kable_format == "latex") {
    return(pdfTable_add_header_above(
      kable_input, header, bold, italic, monospace, underline, strikeout,
      align, color, background, font_size, angle, escape, line, line_sep,
      border_left, border_right))
  }
}

# HTML
htmlTable_add_header_above <- function(kable_input, header, bold, italic,
                                       monospace, underline, strikeout,
                                       align, color, background, font_size,
                                       angle, escape, line, line_sep,
                                       extra_css, include_empty) {
  kable_attrs <- attributes(kable_input)
  important_nodes <- read_kable_as_xml(kable_input)
  body_node <- important_nodes$body
  kable_xml <- important_nodes$table
  kable_xml_thead <- xml_tpart(kable_xml, "thead")

  if (escape) {
    header$header <- escape_html(header$header)
  }

  # If there is no existing header, add one
  if (is.null(kable_xml_thead)) {
    # Add thead node as first child
    xml_add_child(kable_xml, 'thead', .where = 0)
    kable_xml_thead <- xml_tpart(kable_xml, 'thead')

    # To check the number of columns in the new header, compare it to body
    kable_xml_tbody <- xml_tpart(kable_xml, 'tbody')
    if (is.null(kable_xml_tbody))
      body_rows <- kable_ncol <- 0
    else {
      body_rows <- xml_children(kable_xml_tbody)
      kable_ncol <- max(xml_length(body_rows))
    }
  } else {
    header_rows <- xml_children(kable_xml_thead)
    bottom_header_row <- header_rows[[length(header_rows)]]
    kable_ncol <- length(xml_children(bottom_header_row))
  }

  if (sum(header$colspan) != kable_ncol) {
    stop("The new header row you provided has a total of ", sum(header$colspan),
         " columns but the original kable_input has ", kable_ncol, ".")
  }

  new_header_row <- htmlTable_new_header_generator(
    header, bold, italic, monospace, underline, strikeout, align,
    color, background, font_size, angle, line, line_sep, extra_css,
    include_empty, attr(kable_input, 'lightable_class')
  )
  xml_add_child(kable_xml_thead, new_header_row, .where = 0)
  out <- as_kable_xml(body_node)
  if (is.null(kable_attrs$header_above)) {
    kable_attrs$header_above <- 1
  } else {
    kable_attrs$header_above <- kable_attrs$header_above + 1
  }
  attributes(out) <- kable_attrs
  if (!"kableExtra" %in% class(out)) class(out) <- c("kableExtra", class(out))
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
  return(data.frame(header = names(header), colspan = header, row.names = NULL, stringsAsFactors = F))
}

htmlTable_new_header_generator <- function(header_df, bold, italic, monospace,
                                           underline, strikeout, align,
                                           color, background, font_size,
                                           angle, line, line_sep, extra_css,
                                           include_empty, lightable_class) {
  align <- vapply(align, switch_align, 'x', USE.NAMES = FALSE)

  row_style <- paste0(
    "text-align: %s; ",
    ifelse(bold, "font-weight: bold; ", ""),
    ifelse(italic, "font-style: italic; ", ""),
    ifelse(monospace, "font-family: monospace; ", ""),
    ifelse(underline, "text-decoration: underline; ", ""),
    ifelse(strikeout, "text-decoration: line-through; ", "")
  )
  if (!is.null(color)) {
    row_style <- paste0(row_style, "color: ", html_color(color), " !important;")
  }
  if (!is.null(background)) {
    row_style <- paste0(
      row_style,
      "padding-right: 4px; padding-left: 4px; ",
      "background-color: ", html_color(background), " !important;"
    )
  }
  if (!is.null(font_size)) {
    if (is.numeric(font_size)) font_size <- paste0(font_size, "px")
    row_style <- paste0(row_style, "font-size: ", font_size, ";")
  }
  if (!is.null(extra_css)) {
    row_style <- paste0(row_style, extra_css)
  }

  if (!is.null(angle)) {
    angle <- paste0("-webkit-transform: rotate(", angle,
                    "deg); -moz-transform: rotate(", angle,
                    "deg); -ms-transform: rotate(", angle,
                    "deg); -o-transform: rotate(", angle,
                    "deg); transform: rotate(", angle,
                    "deg); display: inline-block; ")
      header_df$header <- ifelse(
        trimws(header_df$header) == "" | include_empty,
        header_df$header,
        paste0('<span style="', angle, '">', header_df$header, '</span>')
      )
  }

  if (is.null(lightable_class)) {
    border_hidden <- 'border-bottom:hidden;'
    line <- ifelse(ez_rep(line, nrow(header_df)),
                   "border-bottom: 1px solid #ddd; padding-bottom: 5px; ", "")
  } else {
    border_hidden <- ''
    if (lightable_class %in% c("lightable-classic", "lightable-classic-2")) {
      line <- ifelse(ez_rep(line, nrow(header_df)),
                     "border-bottom: 1px solid #111111; margin-bottom: -1px; ", "")
    }
    if (lightable_class %in% c("lightable-minimal")) {
      line <- ifelse(ez_rep(line, nrow(header_df)),
                     "border-bottom: 2px solid #00000050; ", "")
    }
    if (lightable_class %in% c("lightable-paper")) {
      line <- ifelse(ez_rep(line, nrow(header_df)),
                     "border-bottom: 1px solid #00000020; padding-bottom: 5px; ", "")
    }
    if (lightable_class %in% c("lightable-material")) {
      line <- ifelse(ez_rep(line, nrow(header_df)),
                     "border-bottom: 1px solid #eee; padding-bottom: 16px; padding-top: 16px; height: 56px;", "")
    }
    if (lightable_class %in% c("lightable-material-dark")) {
      line <- ifelse(ez_rep(line, nrow(header_df)),
                     "border-bottom: 1px solid #FFFFFF12; padding-bottom: 16px; padding-top: 16px; height: 56px;", "")
    }
  }

  line_sep <- ez_rep(line_sep, nrow(header_df))
  line_sep <- paste0('padding-left:', line_sep, 'px;',
                     'padding-right:', line_sep, 'px;')

  row_style <- sprintf(row_style, align)

  header_items <- ifelse(
    trimws(header_df$header) == "" | include_empty,
    paste0('<th style="empty-cells: hide;', border_hidden, '" colspan="', header_df$colspan,
           '"></th>'),
    paste0(
      '<th style="', border_hidden, 'padding-bottom:0; ',
      line_sep, row_style, '" colspan="',
      header_df$colspan, '"><div style="', line, '">', header_df$header,
      '</div></th>')
  )
  header_text <- paste(c("<tr>", header_items, "</tr>"), collapse = "")
  header_xml <- read_xml(header_text, options = c("COMPACT"))
  return(header_xml)
}

# Add an extra header row above the current header in a LaTeX table ------
pdfTable_add_header_above <- function(kable_input, header, bold, italic,
                                      monospace, underline, strikeout, align,
                                      color, background, font_size, angle,
                                      escape, line, line_sep,
                                      border_left, border_right) {
  kable_attrs <- attributes(kable_input)
  table_info <- magic_mirror(kable_input)

  if (is.data.frame(header)){
    if(ncol(header) == 2 & is.character(header[[1]]) & is.numeric(header[[2]])){
      header <- data.frame(header = header[[1]], colspan = header[[2]],
                           stringsAsFactors = FALSE)
    }
    else {
      stop("If header input is provided as a data frame instead of a named vector ",
           "it must consist of only two columns: ",
           "The first should be a character vector with ",
           "header names and the second should be a numeric vector with ",
           "the number of columns the header should span.")
    }
  }
  else {
    header <- standardize_header_input(header)
  }

  if (escape) {
    header$header <- input_escape(header$header, align)
  } else {
    # Issue 836:  backslashes in the replacement
    # need to be escaped.  We can't use fixed() below,
    # because we need the regexp pattern.
    header$header <- gsub("\\\\", "\\\\\\\\", header$header)
  }

  hline_type <- switch(table_info$booktabs + 1, "(\\\\hline)", toprule_regexp)
  new_header_split <- pdfTable_new_header_generator(
    header, table_info$booktabs, bold, italic, monospace, underline, strikeout,
    align, color, background, font_size, angle, line_sep,
    border_left, border_right, line)
  if (any(line)) {
    new_header <- paste0(new_header_split[1], "\n", new_header_split[2])
  } else {
    new_header <- new_header_split[1]
  }
  out <- str_replace(solve_enc(kable_input),
                     hline_type,
                     paste0("\\1\n", new_header))

  # new_header_row <- latex_contents_escape(new_header_split[1])
  if (is.null(table_info$new_header_row)) {
    table_info$new_header_row <- new_header_split[1]
    table_info$header_df <- list(header)
  } else {
    table_info$new_header_row <- c(table_info$new_header_row, new_header_split[1])
    table_info$header_df[[length(table_info$header_df) + 1]] <- header
  }
  out <- finalize_latex(out, kable_attrs, table_info)
  return(out)
}

ez_rep <- function(x, n) {
  if (is.null(x)) return(NULL)
  if (length(x) == 1) return(rep(x, n))
  return(x)
}

pdfTable_new_header_generator <- function(header_df, booktabs = FALSE,
                                          bold, italic, monospace,
                                          underline, strikeout, align,
                                          color, background, font_size, angle,
                                          line_sep, border_left, border_right, line) {
  n <- nrow(header_df)
  bold <- ez_rep(bold, n)
  italic <- ez_rep(italic, n)
  monospace <- ez_rep(monospace, n)
  underline <- ez_rep(underline, n)
  strikeout <- ez_rep(strikeout, n)
  align <- ez_rep(align, n)
  color <- ez_rep(color, n)
  background <- ez_rep(background, n)
  font_size <- ez_rep(font_size, n)
  angle <- ez_rep(angle, n)
  if (!booktabs & n != 1) {
    align[1:(n - 1)] <- paste0(align[1:(n - 1)], "|")
  }
  if (border_left) {
    align[1] <- paste0("|", align[1])
  }
  if (border_right) {
    align[n] <- paste0(align[n], "|")
  }

  header <- header_df$header
  colspan <- header_df$colspan

  header <- ifelse(bold, paste0('\\\\textbf\\{', header, '\\}'), header)
  header <- ifelse(italic, paste0('\\\\em\\{', header, '\\}'), header)
  header <- ifelse(monospace, paste0('\\\\ttfamily\\{', header, '\\}'), header)
  header <- ifelse(underline, paste0('\\\\underline\\{', header, '\\}'), header)
  header <- ifelse(strikeout, paste0('\\\\sout\\{', header, '\\}'), header)
  if (!is.null(color)) {
    color <- latex_color(color)
    header <- paste0("\\\\textcolor", color, "\\{", header, "\\}")
  }
  if (!is.null(background)) {
    background <- latex_color(background)
    header <- paste0("\\\\cellcolor", background, "\\{", header, "\\}")
  }
  if (!is.null(font_size)) {
    header <- paste0("\\\\bgroup\\\\fontsize\\{", font_size, "\\}\\{",
                     as.numeric(font_size) + 2,
                     "\\}\\\\selectfont ", header, "\\\\egroup\\{\\}")
  }
  if (!is.null(angle)) {
    header <- paste0("\\\\rotatebox\\{", angle, "\\}\\{", header, "\\}")
  }
  header_items <- paste0(
    '\\\\multicolumn\\{', colspan, '\\}\\{', align, '\\}\\{', header, '\\}'
  )

  header_text <- paste(paste(header_items, collapse = " & "), "\\\\\\\\")
  cline <- cline_gen(header_df, booktabs, line_sep, line)
  return(c(header_text, cline))
}

cline_gen <- function(header_df, booktabs, line_sep, line) {
  cline_end <- cumsum(header_df$colspan)
  cline_start <- c(0, cline_end) + 1
  cline_start <- cline_start[-length(cline_start)]
  cline_type <- switch(
    booktabs + 1,
    "\\\\cline{",
    paste0("\\\\cmidrule(l{", line_sep, "pt}r{", line_sep, "pt}){"))
  cline <- paste0(cline_type, cline_start, "-", cline_end, "}")
  line <- rep_len(line, length(cline))
  keep <- trimws(header_df$header) != ""
  cline <- cline[keep]
  line <- line[keep]
  cline <- paste(cline[line], collapse = " ")
  return(cline)
}

switch_align <- function(x) {
  if (x %in% c('l', 'c', 'r')) {
    return(switch(x, l = 'left', c = 'center', r = 'right'))
  }
  return(x)
}

