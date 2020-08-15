#' Specify the look of the selected column
#'
#' @description This function allows users to select a column and then specify
#' its look.
#'
#' @param kable_input Output of `knitr::kable()` with `format` specified
#' @param column A numeric value or vector indicating which column(s) to be selected.
#' @param width A character string telling HTML & LaTeX how wide the column
#' needs to be, e.g. "10cm", "3in" or "30em".
#' @param bold A T/F value to control whether the text of the selected column
#' need to be bolded.
#' @param italic A T/F value to control whether the text of the selected column
#' need to be emphasized.
#' @param monospace A T/F value to control whether the text of the selected column
#' need to be monospaced (verbatim)
#' @param underline A T/F value to control whether the text of the selected row
#' need to be underlined
#' @param strikeout A T/F value to control whether the text of the selected row
#' need to be stricked out.
#' @param color A character string for column text color. Here please pay
#' attention to the differences in color codes between HTML and LaTeX.
#' @param background A character string for column background color. Here please
#' pay attention to the differences in color codes between HTML and LaTeX.
#' @param border_left A logical variable indicating whether there should be a
#' border line on the left of the selected column. In HTML, you can also pass
#' in a character string for the CSS of the border line
#' @param border_right A logical variable indicating whether there should be a
#' border line on the right of the selected column. In HTML, you can also pass
#' in a character string for the CSS of the border line
#' @param width_min Only for HTML table. Normal column width will automatically
#' collapse when the window cannot hold enough contents. With this `width_min`,
#' you can set up a column with a width that won't collapse even when the
#' window is not wide enough.
#' @param width_max Only for HTML table. `width_max` defines the maximum width
#' of table columns.
#' @param extra_css Extra css text to be passed into the cells of the row. Note
#' that it's not for the whole column but to each individual cells
#' @param include_thead T/F. A HTML only feature to contoll whether the
#' header row will be manipulated. Default is `FALSE`.
#' @param latex_column_spec Only for LaTeX tables.  Code to replace the column
#' specification.  If not `NULL`, will override all other arguments.
#' @param latex_valign vertical alignment. Only works when you specified column
#'  width. Choose among `p`, `m`, `b`.
#'
#' @details Use `latex_column_spec` in a LaTeX table to change or
#' customize the column specification.  Because of the way it is handled
#' internally, any backslashes must be escaped.
#'
#' @examples x <- knitr::kable(head(mtcars), "html")
#' column_spec(x, 1:2, width = "20em", bold = TRUE, italic = TRUE)
#' x <- knitr::kable(head(mtcars), "latex", booktabs = TRUE)
#' column_spec(x, 1, latex_column_spec = ">{\\\\color{red}}c")
#' @export
column_spec <- function(kable_input, column,
                        width = NULL, bold = FALSE, italic = FALSE,
                        monospace = FALSE, underline = FALSE, strikeout = FALSE,
                        color = NULL, background = NULL,
                        border_left = FALSE, border_right = FALSE,
                        width_min = NULL, width_max = NULL,
                        extra_css = NULL, include_thead = FALSE,
                        latex_column_spec = NULL, latex_valign = 'p') {
  if (!is.numeric(column)) {
    stop("column must be numeric. ")
  }
  kable_format <- attr(kable_input, "format")
  if (!kable_format %in% c("html", "latex")) {
    warning("Please specify format in kable. kableExtra can customize either ",
            "HTML or LaTeX outputs. See https://haozhu233.github.io/kableExtra/ ",
            "for details.")
    return(kable_input)
  }
  if (kable_format == "html") {
    return(column_spec_html(kable_input, column, width,
                            bold, italic, monospace,
                            underline, strikeout,
                            color, background,
                            border_left, border_right,
                            width_min, width_max,
                            extra_css, include_thead))
  }
  if (kable_format == "latex") {
    return(column_spec_latex(kable_input, column, width,
                             bold, italic, monospace,
                             underline, strikeout,
                             color, background,
                             border_left, border_right,
                             latex_column_spec = latex_column_spec,
                             latex_valign = latex_valign))
  }
}

column_spec_html <- function(kable_input, column, width,
                             bold, italic, monospace,
                             underline, strikeout,
                             color, background,
                             border_left, border_right,
                             width_min, width_max,
                             extra_css, include_thead) {
  kable_attrs <- attributes(kable_input)
  kable_xml <- read_kable_as_xml(kable_input)
  kable_tbody <- xml_tpart(kable_xml, "tbody")

  group_header_rows <- attr(kable_input, "group_header_rows")
  all_contents_rows <- seq(1, length(xml_children(kable_tbody)))

  if (!is.null(group_header_rows)) {
    all_contents_rows <- all_contents_rows[!all_contents_rows %in%
                                             group_header_rows]
  }

  # Border css
  border_l_css <- "1px solid"
  border_r_css <- "1px solid"
  if (is.character(border_left)) {
    border_l_css <- border_left
    border_left <- T
  }
  if (is.character(border_right)) {
    border_r_css <- border_right
    border_right <- T
  }

  for (i in all_contents_rows) {
    for (j in column) {
      target_cell <- xml_child(xml_child(kable_tbody, i), j)
      column_spec_html_cell(
        target_cell, width, width_min, width_max,
        bold, italic, monospace, underline, strikeout,
        color, background, border_left, border_right,
        border_l_css, border_r_css,
        extra_css
      )
    }
  }

  if (include_thead) {
    kable_thead <- xml_tpart(kable_xml, "thead")
    nrow_thead <- length(xml_children(kable_thead))
    for (j in column) {
      target_cell <- xml_child(xml_child(kable_thead, nrow_thead), j)
      column_spec_html_cell(
        target_cell, width, width_min, width_max,
        bold, italic, monospace, underline, strikeout,
        color, background, border_left, border_right,
        border_l_css, border_r_css,
        extra_css
      )
    }
  }

  out <- as_kable_xml(kable_xml)
  attributes(out) <- kable_attrs
  if (!"kableExtra" %in% class(out)) class(out) <- c("kableExtra", class(out))
  return(out)
}

column_spec_html_cell <- function(target_cell, width, width_min, width_max,
                                  bold, italic, monospace, underline, strikeout,
                                  color, background,
                                  border_left, border_right,
                                  border_l_css, border_r_css,
                                  extra_css) {
  if (is.na(xml_attr(target_cell, "style"))) {
    xml_attr(target_cell, "style") <- ""
  }
  if (!is.null(width)) {
    xml_attr(target_cell, "style") <- paste0(xml_attr(target_cell, "style"),
                                             "width: ", width, "; ")
  }
  if (!is.null(width_min)) {
    xml_attr(target_cell, "style") <- paste0(xml_attr(target_cell, "style"),
                                             "min-width: ", width_min, "; ")
  }
  if (!is.null(width_max)) {
    xml_attr(target_cell, "style") <- paste0(xml_attr(target_cell, "style"),
                                             "max-width: ", width_max, "; ")
  }
  if (bold) {
    xml_attr(target_cell, "style") <- paste0(xml_attr(target_cell, "style"),
                                             "font-weight: bold;")
  }
  if (italic) {
    xml_attr(target_cell, "style") <- paste0(xml_attr(target_cell, "style"),
                                             "font-style: italic;")
  }
  if (monospace) {
    xml_attr(target_cell, "style") <- paste0(xml_attr(target_cell, "style"),
                                             "font-family: monospace;")
  }
  if (underline) {
    xml_attr(target_cell, "style") <- paste0(xml_attr(target_cell, "style"),
                                             "text-decoration: underline;")
  }
  if (strikeout) {
    xml_attr(target_cell, "style") <- paste0(xml_attr(target_cell, "style"),
                                             "text-decoration: line-through;")
  }
  if (!is.null(color)) {
    xml_attr(target_cell, "style") <- paste0(xml_attr(target_cell, "style"),
                                             "color: ", html_color(color),
                                             " !important;")
  }
  if (!is.null(background)) {
    xml_attr(target_cell, "style") <- paste0(xml_attr(target_cell, "style"),
                                             "background-color: ",
                                             html_color(background),
                                             " !important;")
  }
  if (border_left) {
    xml_attr(target_cell, "style") <- paste0(xml_attr(target_cell, "style"),
                                             "border-left:", border_l_css, ";")
  }
  if (border_right) {
    xml_attr(target_cell, "style") <- paste0(xml_attr(target_cell, "style"),
                                             "border-right:", border_r_css, ";")
  }
  if (!is.null(extra_css)) {
    xml_attr(target_cell, "style") <- paste0(xml_attr(target_cell, "style"),
                                             extra_css)
  }
}

column_spec_latex <- function(kable_input, column, width,
                              bold, italic, monospace,
                              underline, strikeout,
                              color, background,
                              border_left, border_right,
                              latex_column_spec, latex_valign) {
  table_info <- magic_mirror(kable_input)
  if (!is.null(table_info$collapse_rows)) {
    message("Usually it is recommended to use column_spec before collapse_rows,",
            " especially in LaTeX, to get a desired result. ")
  }
  align_collapse <- ifelse(table_info$booktabs | !is.null(table_info$xtable),
                           "", "\\|")
  kable_align_old <- paste(table_info$align_vector, collapse = align_collapse)

  table_info$align_vector[column] <- unlist(lapply(
    table_info$align_vector_origin[column],
    function(x) {
      latex_column_align_builder(
        x, width, bold, italic, monospace, underline, strikeout,
        color, background, border_left, border_right, latex_column_spec,
        latex_valign)
    }
  ))

  kable_align_new <- paste(table_info$align_vector, collapse = align_collapse)

  out <- sub(paste0("\\{", kable_align_old, "\\}"),
             paste0("\\{", kable_align_new, "\\}"),
             solve_enc(kable_input),
             perl = T)

  if (!is.null(width)) {
    fix_newline <- replace_makecell_with_newline(out, table_info, column)
    out <- fix_newline[[1]]
    table_info <- fix_newline[[2]]
  }

  out <- structure(out, format = "latex", class = "knitr_kable")
  if (!is.null(width)) {
    if (is.null(table_info$column_width)) {
      table_info$column_width <- list()
    }
    for (i in column) {
      table_info$column_width[[paste0("column_", i)]] <- width
    }
  }
  attr(out, "kable_meta") <- table_info
  return(out)
}

latex_column_align_builder <- function(x, width, bold, italic, monospace,
                                       underline, strikeout,
                                       color, background,
                                       border_left, border_right,
                                       latex_column_spec, latex_valign) {
  extra_align <- ""
  if (!is.null(width)) {
    extra_align <- switch(x,
                          "l" = "\\\\raggedright\\\\arraybackslash",
                          "c" = "\\\\centering\\\\arraybackslash",
                          "r" = "\\\\raggedleft\\\\arraybackslash")
    x <- paste0(latex_valign, "\\{", width, "\\}")
  }

  if (!is.null(color)) {
    color <- paste0("\\\\leavevmode\\\\color", latex_color(color))
  }

  if (!is.null(background)) {
    background <- paste0("\\\\columncolor", latex_color(background))
  }

  latex_array_options <- c("\\\\bfseries", "\\\\em", "\\\\ttfamily",
                           "\\\\underline", "\\\\sout")[
                             c(bold, italic, monospace, underline, strikeout)]
  latex_array_options <- c(latex_array_options, extra_align,
                           color, background)
  latex_array_options <- paste0(
    "\\>\\{", paste(latex_array_options, collapse = ""), "\\}"
  )
  x <- paste0(latex_array_options, x)
  if (border_left) {
    x <- paste0("\\|", x)
  }
  if (border_right) {
    x <- paste0(x, "\\|")
  }
  if (!is.null(latex_column_spec))
  x <- latex_column_spec

  return(x)
}

replace_makecell_with_newline <- function(kable_input, table_info, column) {
  if (!str_detect(kable_input, "makecell")) return(list(kable_input, table_info))
  contents_table <- data.frame(sapply(table_info$contents,
                           function(x) {str_split(x, " \\& ")[[1]]}),
                           stringsAsFactors = F)
  names(contents_table) <- paste0("x", 1:table_info$nrow)
  rows_check_makecell <- str_detect(contents_table[column, ], "makecell")
  if (sum(rows_check_makecell) == 0) return(list(kable_input, table_info))
  rows_to_replace <- which(rows_check_makecell)

  for (i in column) {
    target_column <- contents_table[i, ]
    for (j in which(str_detect(target_column, "\\\\\\\\makecell"))) {
      contents_table[i, j] <- str_replace(
        contents_table[i, j], "\\\\\\\\makecell\\\\\\[.\\\\\\]\\\\\\{", "")
      contents_table[i, j] <- str_replace(
        contents_table[i, j], "\\\\\\}$", "")
      contents_table[i, j] <- str_replace_all(
        contents_table[i, j], "\\\\\\\\\\\\\\\\", "\\\\\\\\newline "
      )
    }
  }

  new_contents <- unlist(lapply(contents_table, paste, collapse = " & "))
  for (i in rows_to_replace) {
    kable_input <- sub(table_info$contents[i], new_contents[i], kable_input,
                       perl = T)
    table_info$contents[i] <- new_contents[i]
  }

  return(list(kable_input, table_info))
}
