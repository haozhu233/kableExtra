#' Put a few rows of a table into one category
#'
#' @description Group a few rows in a table together under a label.
#'
#' @param kable_input Output of `knitr::kable()` with `format` specified
#' @param group_label A character string for the name of the group
#' @param start_row A numeric value that tells the function in which row the
#' group starts. Note that the counting excludes header rows and other group
#' labeling rows
#' @param end_row A numeric value that tells the function in which row the group
#' ends.
#' @param index A named vector providing the index for robust row-grouping tasks.
#' Basically, you can use it in the same way as `add_header_above()`.
#' @param label_row_css A character string for any customized css used for the
#' labeling row. By default, the labeling row will have a solid black line
#' underneath. Only useful for HTML documents.
#' @param latex_gap_space A character value telling LaTeX how large the gap
#' between the previous row and the group labeling row. Only useful for LaTeX
#' documents.
#' @param escape A T/F value showing whether special characters should be
#' escaped.
#' @param latex_align Adjust justification of group_label in latex only. Value should be "c" for
#' centered on row, "r" for right justification, or "l" for left justification. Default
#' Value is "l"  If using html, the alignment can be set by using the label_row_css
#' parameter.
#' @param latex_wrap_text T/F for wrapping long text. Default is off. Whenever
#' it is turned on, the table will take up the entire line. It's recommended
#' to use this with full_width in kable_styling.
#' @param colnum A numeric that determines how many columns the text should span.
#' The default setting will have the text span the entire length.
#' @param bold A T/F value to control whether the text should be bolded.
#' @param italic A T/F value to control whether the text should to be emphasized.
#' @param hline_before A T/F value that adds a horizontal line before the group_row label.  Default
#' value is False.
#' @param hline_after A replicate of `hline.after` in xtable. It
#' adds a hline after the row
#' @param extra_latex_after Extra LaTeX text to be added after the row.
#' @param indent A T/F value to control whether list items are indented.
#' @param monospace T/F value to control whether the text of the
#' selected column need to be monospaced (verbatim)
#' @param underline T/F value to control whether the text of the
#' selected row need to be underlined
#' @param strikeout T/F value to control whether the text of the
#' selected row need to be struck out.
#' @param color A character string for column text color. Here please
#' pay attention to the differences in color codes between HTML and LaTeX.
#' @param background A character string for column background color. Here please
#' pay attention to the differences in color codes between HTML and LaTeX.
#'
#' @details
#' In HTML output, it is an error to insert a break
#' between groups that would fall within collapsed rows
#' produced by [collapse_rows()].  To work around
#' this, modify the column entries that follow the break
#' to be different from those before the break, e.g.
#' by adding a zero-width space (Unicode `"\u200B"`)
#' to those in the later group, so that [collapse_rows()]
#' will think the entries are different and will not
#' collapse them together.
#'
#' @examples
#' \dontrun{
#' x <- knitr::kable(head(mtcars), "html")
#' # Put Row 2 to Row 5 into a Group and label it as "Group A"
#' pack_rows(x, "Group A", 2, 5)
#' }
#'
#' @export
group_rows <- function(kable_input, group_label = NULL,
                       start_row = NULL, end_row = NULL,
                       index = NULL,
                       label_row_css = "border-bottom: 1px solid;",
                       latex_gap_space = "0.3em",
                       escape = TRUE, latex_align = "l",
                       latex_wrap_text = FALSE,
                       colnum = NULL,
                       bold = TRUE,
                       italic = FALSE,
                       hline_before = FALSE,
                       hline_after = FALSE,
                       extra_latex_after = NULL,
                       indent = TRUE,
                       monospace = FALSE, underline = FALSE, strikeout = FALSE,
                       color = NULL, background = NULL) {

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

  if (is.null(index)) {
    if (kable_format == "html") {
      if (!missing(latex_align)) warning("latex_align parameter is not used in HTML Mode,
                                    use label_row_css instead.")
      return(group_rows_html(kable_input, group_label, start_row, end_row,
                             label_row_css, escape, colnum, indent,
                             bold, italic, monospace, underline, strikeout,
                             color, background))}
    if (kable_format == "latex") {
      return(group_rows_latex(kable_input, group_label, start_row, end_row,
                              latex_gap_space, escape, latex_align, colnum,
                              bold, italic, hline_before, hline_after,
                              extra_latex_after, indent, latex_wrap_text,
                              monospace, underline, strikeout,
                              color, background))
    }
  } else {
    index <- group_row_index_translator(index)
    out <- kable_input
    if (kable_format == "html") {
      for (i in 1:nrow(index)) {
        if (!missing(latex_align)) warning("latex_align parameter is not used in HTML Mode,
                                    use label_row_css instead.")
        out <- group_rows_html(out, index$header[i],
                               index$start[i], index$end[i],
                               label_row_css, escape, colnum, indent,
                               bold, italic, monospace, underline, strikeout,
                               color, background)
      }
    }
    if (kable_format == "latex") {
      for (i in 1:nrow(index)) {
        out <- group_rows_latex(out, index$header[i],
                               index$start[i], index$end[i],
                               latex_gap_space, escape, latex_align, colnum,
                               bold, italic, hline_before, hline_after,
                               extra_latex_after, indent, latex_wrap_text,
                               monospace, underline, strikeout,
                               color, background)
      }
    }
    return(out)
  }
}

group_row_index_translator <- function(index) {
  index <- standardize_header_input(index)
  index$start <- cumsum(c(1, index$colspan))[1:length(index$colspan)]
  index$end <- cumsum(index$colspan)
  index$header <- trimws(regex_unescape(index$header))
  index <- index[index$header != "", ]
  return(index)
}

group_rows_html <- function(kable_input, group_label, start_row, end_row,
                            label_row_css, escape, colnum, indent,
                            bold, italic, monospace, underline, strikeout,
                            color, background) {
  kable_attrs <- attributes(kable_input)

  collapse_matrix <- kable_attrs$collapse_matrix
  if (!is.null(collapse_matrix) &&
      any(collapse_matrix[start_row,] == 0))
    stop("Grouping occurs within collapsed rows at row ", start_row)

  important_nodes <- read_kable_as_xml(kable_input)
  body_node <- important_nodes$body
  kable_xml <- important_nodes$table
  kable_tbody <- xml_tpart(kable_xml, "tbody")
  if (is.null(kable_tbody))
    return(kable_input)
  if (escape) {
    group_label <- escape_html(group_label)
  }

  group_header_rows <- attr(kable_input, "group_header_rows")
  group_seq <- seq(start_row, end_row)
  if (!is.null(group_header_rows)) {
    group_seq <- positions_corrector(group_seq, group_header_rows,
                                     length(xml_children(kable_tbody)))
    # Update the old group_header_rows attribute with their new positions
    kable_attrs$group_header_rows <- ifelse(kable_attrs$group_header_rows > group_seq[1],
                                            kable_attrs$group_header_rows+1,
                                            kable_attrs$group_header_rows)
  }

  # Insert a group header row
  starting_node <- xml_child(kable_tbody, group_seq[1])
  kable_ncol <- ifelse(is.null(colnum),
                       length(xml_children(starting_node)),
                       colnum)

  if (bold) group_label <- paste0("<strong>", group_label, "</strong>")
  if (italic) group_label <- paste0("<em>", group_label, "</em>")

  if (label_row_css == "border-bottom: 1px solid;") {
    if (!is.null(attr(kable_input, "lightable_class"))) {
      lightable_class <- attr(kable_input, "lightable_class")
      if (lightable_class %in% c(
        "lightable-classic", "lightable-classic-2", "lightable-minimal")) {
        label_row_css <- "border-bottom: 0;"
      }
      if (lightable_class %in% c("lightable-paper")) {
        label_row_css <- "border-bottom: 1px solid #00000020;"
      }
      if (lightable_class %in% c("lightable-material")) {
        label_row_css <- "border-bottom: 1px solid #eee; "
      }
      if (lightable_class %in% c("lightable-material-dark")) {
        label_row_css <- "border-bottom: 1px solid #FFFFFF12; color: #FFFFFF60;"
      }
    }
  }
  if (monospace) {
    label_row_css <- paste0(label_row_css, "font-family: monospace;")
  }
  if (underline) {
    label_row_css <- paste0(label_row_css, "text-decoration: underline;")
  }
  if (strikeout) {
    label_row_css <- paste0(label_row_css, "text-decoration: line-through;")
  }
  if (!is.null(color)) {
    label_row_css <- paste0(label_row_css, "color: ", html_color(color),
                            " !important;")
  }
  if (!is.null(background)) {
    label_row_css <- paste0(label_row_css, "background-color: ",
                            html_color(background), " !important;")
  }

  group_header_row_text <- paste0(
    '<tr groupLength="', length(group_seq), '"><td colspan="', kable_ncol,
    '" style="', label_row_css, '">', group_label, "</td></tr>")

  group_header_row <- read_xml(group_header_row_text, options = "COMPACT")
  xml_add_sibling(starting_node, group_header_row, .where = "before")

  # add indentations to items
  out <- as_kable_xml(body_node)
  attributes(out) <- kable_attrs
  attr(out, "group_header_rows") <- c(attr(out, "group_header_rows"), group_seq[1])
  if (indent) {
    out <- add_indent_html(out, positions = seq(start_row, end_row))
  }
  return(out)
}

group_rows_latex <- function(kable_input, group_label, start_row, end_row,
                             gap_space, escape, latex_align, colnum,
                             bold = T, italic = F, hline_before = F, hline_after = F,
                             extra_latex_after = NULL, indent, latex_wrap_text = F,
                             monospace = F, underline = F, strikeout = F,
                             color = NULL, background = NULL) {
  kable_attrs <- attributes(kable_input)
  table_info <- magic_mirror(kable_input)
  out <- solve_enc(kable_input)

  if (table_info$duplicated_rows) {
    dup_fx_out <- fix_duplicated_rows_latex(out, table_info)
    out <- dup_fx_out[[1]]
    table_info <- dup_fx_out[[2]]
  }

  if (escape) {
    group_label <- input_escape(group_label, latex_align)
  } else {
    group_label <- sim_all_double_escape(group_label)
  }

  if (bold) {
    group_label <- paste0("\\\\textbf{", group_label, "}")
  }

  if (italic) group_label <- paste0("\\\\textit{", group_label, "}")

  if (monospace) {
    group_label <- paste0("\\\\ttfamily\\{", group_label, "\\}")
  }
  if (underline) {
    group_label <- paste0("\\\\underline\\{", group_label, "\\}")
  }
  if (strikeout) {
    group_label <- paste0("\\\\sout\\{", group_label, "\\}")
  }
  if (!is.null(color)) {
    group_label <- paste0("\\\\textcolor", latex_color(color), "\\{",
                              group_label, "\\}")
  }
  if (!is.null(background)) {
    group_label <- paste0("\\\\cellcolor", latex_color(background), "\\{",
                              group_label, "\\}")
  }
  # Add group label
  if (latex_wrap_text) {
    latex_align <- switch(
      latex_align,
      "l" = "p{\\\\linewidth}",
      "c" = ">{\\\\centering\\\\arraybackslash}p{\\\\linewidth}",
      "r" = ">{\\\\centering\\\\arraybackslash}p{\\\\linewidth}"
    )
  }


  rowtext <- table_info$contents[start_row + table_info$position_offset]
  if (table_info$booktabs) {
    pre_rowtext <- paste0("\\\\addlinespace[", gap_space, "]\n")
  } else {
    pre_rowtext <- ''
    hline_after <- TRUE
  }
  pre_rowtext <- paste0(
    pre_rowtext,
    ifelse(hline_before,"\\\\hline\n", ""),
    "\\\\multicolumn{", ifelse(is.null(colnum),
                               table_info$ncol,
                               colnum),
    "}{", latex_align,"}{", group_label,
    "}\\\\\\\\\n", ifelse(hline_after, "\\\\hline\n", '')
  )
  if(!is.null(extra_latex_after)){
    pre_rowtext <- paste0(pre_rowtext,
                      regex_escape(extra_latex_after, double_backslash = TRUE))
  }
  new_rowtext <- paste0(pre_rowtext, rowtext)
  if (start_row + 1 == table_info$nrow &
      !is.null(table_info$repeat_header_latex) & table_info$booktabs) {
    out <- sub(paste0(rowtext, "\\\\\\\\\\*\n"),
               paste0(new_rowtext, "\\\\\\\\\\*\n"),
               out)
  } else {
    out <- sub(paste0(rowtext, "\\\\\\\\\n"),
               paste0(new_rowtext, "\\\\\\\\\n"),
               out)
  }

  out <- gsub("\\\\addlinespace\n", "", out)

  table_info$group_rows_used <- TRUE

  out <- finalize_latex(out, kable_attrs, table_info)

  if (indent) {
    out <- add_indent_latex(out, seq(start_row, end_row))
  }
  return(out)
}

#' Automatically figuring out the group_row index
#'
#' @description This helper function allows users to build the `group_row`
#' index more quickly and use `group_rows` in a way that is similar with
#' `collapse_rows`.
#'
#' @param x The index column. A vector. For example `c("a", "a", "b", "b", "b")``
#'
#' @export
auto_index <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  x_rle <- rle(x)
  index <- x_rle$lengths
  names(index) <- x_rle$values
  return(index)
}

#' @rdname group_rows
#' @export
pack_rows <- group_rows
