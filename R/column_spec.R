#' Specify the look of the selected column
#'
#' @description This function allows users to select a column and then specify
#' its look.
#'
#' @param kable_input Output of `knitr::kable()` with `format` specified
#' @param column A numeric value or vector indicating which column(s) to be selected.
#' @param width A character string telling HTML & LaTeX how wide the column
#' needs to be, e.g. "10cm", "3in" or "30em".
#' @param bold T/F value or vector to control whether the text of the selected
#' column need to be bolded.
#' @param italic T/F value or vector to control whether the text of the
#' selected column need to be emphasized.
#' @param monospace T/F value or vector to control whether the text of the
#' selected column need to be monospaced (verbatim)
#' @param underline T/F value or vector to control whether the text of the
#' selected row need to be underlined
#' @param strikeout T/F value or vector to control whether the text of the
#' selected row need to be struck out.
#' @param color A character string or vector for column text color. Here please
#' pay attention to the differences in color codes between HTML and LaTeX.
#' @param background A character string or vector for column background color. Here please
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
#' @param extra_css A vector of extra css text to be passed into the cells of
#' the column.
#' @param include_thead T/F. A HTML only feature to control whether the
#' header row will be manipulated. Default is `FALSE`.
#' @param latex_column_spec Only for LaTeX tables.  Code to replace the column
#' specification.  If not `NULL`, will override all other arguments.
#' @param latex_valign vertical alignment. Only works when you specified column
#'  width. Choose among `p`, `m`, `b`.
#' @param link A vector of strings for url links.
#' @param new_tab T/F for whether to open up the new link in new tab
#' @param tooltip A vector of strings to be displayed as tooltip.
#' Obviously, this feature is only available in HTML. Read the package
#' vignette to see how to use bootstrap tooltip css to improve the loading
#' speed and look.
#' @param popover Similar with tooltip but can hold more contents. The best way
#' to build a popover is through `spec_popover()`. If you only provide a text
#' string, it will be used as content. Note that You have to enable this
#' bootstrap module manually. Read the package vignette to see how.
#' @param image Vector of image paths.
#'
#' @details Use `latex_column_spec` in a LaTeX table to change or
#' customize the column specification.  Because of the way it is handled
#' internally, any backslashes must be escaped.
#'
#' @examples
#' \dontrun{
#' x <- knitr::kable(head(mtcars), "html")
#' column_spec(x, 1:2, width = "20em", bold = TRUE, italic = TRUE)
#' x <- knitr::kable(head(mtcars), "latex", booktabs = TRUE)
#' column_spec(x, 1, latex_column_spec = ">{\\\\color{red}}c")
#' }
#' @export
column_spec <- function(kable_input, column,
                        width = NULL, bold = FALSE, italic = FALSE,
                        monospace = FALSE, underline = FALSE, strikeout = FALSE,
                        color = NULL, background = NULL,
                        border_left = FALSE, border_right = FALSE,
                        width_min = NULL, width_max = NULL,
                        extra_css = NULL, include_thead = FALSE,
                        latex_column_spec = NULL, latex_valign = 'p',
                        link = NULL, new_tab = TRUE,
                        tooltip = NULL, popover = NULL, image = NULL) {
  if (!is.numeric(column)) {
    stop("column must be numeric. ")
  }
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
  if (kable_format == "html") {
    return(column_spec_html(kable_input, column, width,
                            bold, italic, monospace,
                            underline, strikeout,
                            color, background,
                            border_left, border_right,
                            width_min, width_max,
                            extra_css, include_thead,
                            link, new_tab, tooltip, popover, image))
  }
  if (kable_format == "latex") {
    return(column_spec_latex(kable_input, column, width,
                             bold, italic, monospace,
                             underline, strikeout,
                             color, background,
                             border_left, border_right,
                             latex_column_spec, latex_valign, include_thead,
                             link, image))
  }
}

column_spec_html <- function(kable_input, column, width,
                             bold, italic, monospace,
                             underline, strikeout,
                             color, background,
                             border_left, border_right,
                             width_min, width_max,
                             extra_css, include_thead,
                             link, new_tab, tooltip, popover, image) {
  kable_attrs <- attributes(kable_input)
  important_nodes <- read_kable_as_xml(kable_input)
  body_node <- important_nodes$body
  kable_xml <- important_nodes$table
  kable_tbody <- xml_tpart(kable_xml, "tbody")
  if (is.null(kable_tbody))
    return(kable_input)

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

  off <- 0
  if (include_thead) {
    kable_thead <- xml_tpart(kable_xml, "thead")
    if (!is.null(kable_thead)) {
      nrows <- length(all_contents_rows) + 1
      off <- 1

      bold <- ensure_len_html(bold, nrows, "bold")
      italic <- ensure_len_html(italic, nrows, "italic")
      monospace <- ensure_len_html(monospace, nrows, "monospace")
      underline <- ensure_len_html(underline, nrows, "underline")
      strikeout <- ensure_len_html(strikeout, nrows, "strikeout")
      color <- ensure_len_html(color, nrows, "color")
      background <- ensure_len_html(background, nrows,"background")
      link <- ensure_len_html(link, nrows, "link")
      new_tab <- ensure_len_html(new_tab, nrows, "new_tab")
      tooltip <- ensure_len_html(tooltip, nrows, "tooltip")
      popover <- ensure_len_html(popover, nrows, "popover")
      image <- ensure_len_html(image, nrows, "image")

      nrow_thead <- length(xml_children(kable_thead))
      for (j in column) {
        target_cell <- xml_child(xml_child(kable_thead, nrow_thead), j)
        column_spec_html_cell(
          target_cell, width, width_min, width_max,
          bold[1], italic[1], monospace[1], underline[1], strikeout[1],
          color[1], background[1], border_left, border_right,
          border_l_css, border_r_css,
          extra_css,
          link[1], new_tab[1], tooltip[1], popover[1], image[1]
        )
      }
    }
  } else {
    nrows <- length(all_contents_rows)

    bold <- ensure_len_html(bold, nrows, "bold")
    italic <- ensure_len_html(italic, nrows, "italic")
    monospace <- ensure_len_html(monospace, nrows, "monospace")
    underline <- ensure_len_html(underline, nrows, "underline")
    strikeout <- ensure_len_html(strikeout, nrows, "strikeout")
    color <- ensure_len_html(color, nrows, "color")
    background <- ensure_len_html(background, nrows,"background")
    link <- ensure_len_html(link, nrows, "link")
    new_tab <- ensure_len_html(new_tab, nrows, "new_tab")
    tooltip <- ensure_len_html(tooltip, nrows, "tooltip")
    popover <- ensure_len_html(popover, nrows, "popover")
    image <- ensure_len_html(image, nrows, "image")
  }

  for (i in seq(length(all_contents_rows))) {
    for (j in column) {
      io <- i + off
      target_cell <- xml_child(xml_child(kable_tbody, all_contents_rows[i]), j)
      column_spec_html_cell(
        target_cell, width, width_min, width_max,
        bold[io], italic[io], monospace[io], underline[io], strikeout[io],
        color[io], background[io], border_left, border_right,
        border_l_css, border_r_css,
        extra_css,
        link[io], new_tab[io], tooltip[io], popover[io], image[io]
      )
    }
  }

  out <- as_kable_xml(body_node)
  attributes(out) <- kable_attrs
  if (!"kableExtra" %in% class(out)) class(out) <- c("kableExtra", class(out))
  return(out)
}

ensure_len_html <- function(x, l, name) {
  if (is.null(x)) return(NULL)
  if (length(x) == 1) return(rep(x, l))
  if (length(x) == l) return(x)
  warning("The number of provided values in ", name, " does not equal to the ",
          "number of rows. ")
  return(rep(x, ceiling(l / length(x)))[seq(1, l)])
}

column_spec_html_cell <- function(target_cell, width, width_min, width_max,
                                  bold, italic, monospace, underline, strikeout,
                                  color, background,
                                  border_left, border_right,
                                  border_l_css, border_r_css,
                                  extra_css,
                                  link, new_tab, tooltip, popover, image) {
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
                                             enc2utf8(extra_css))
  }

  if (!is.null(image) && (length(image) > 1 || !is.null(image[[1]]))) {
    image <- image[[1]]
    if (inherits(image, "kableExtraInlinePlots")) {
      if (!is.null(image$svg_text)) {
        xml_add_child(target_cell, xml2::read_xml(image$svg_text))
      } else {
        img_text <- paste0('<img src="', image$path, '" width="',
        image$width / image$res * 96, '" height="',
        image$height / image$res * 96,
        '"></img>')
        xml_add_child(target_cell, xml2::read_html(img_text))
      }
    } else {
      img_text <- paste0('<img src="', image, '"></img>')
      xml_add_child(target_cell, xml2::read_html(img_text))
    }
  }

  # favor popover over tooltip
  if (!is.null(popover)) {
    if (!inherits(popover, "ke_popover")) popover <- spec_popover(popover)
    popover_list <- lapply(attr(popover, 'list'), enc2utf8)
    for (p in names(popover_list)) {
      xml_attr(target_cell, p) <- popover_list[p]
    }
  } else if (!is.null(tooltip)) {
    if (!inherits(tooltip, "ke_tooltip")) tooltip <- spec_tooltip(tooltip)
    tooltip_list <- lapply(attr(tooltip, 'list'), enc2utf8)
    for (t in names(tooltip_list)) {
      xml_attr(target_cell, t) <- tooltip_list[t]
    }
  }

  if (!is.null(link)) {
    href_node <- xml2::read_xml(paste0(
      '<a href="', link, '">', xml_text(target_cell), '</a>'
    ))
    if (!is.null(color)) {
      xml_attr(href_node, "style") <- paste0("color: ", html_color(color),
                                        " !important;")
    }
    xml_add_child(target_cell, href_node)
    xml_text(target_cell) <- ""
  }
}

column_spec_latex <- function(kable_input, column, width,
                              bold, italic, monospace,
                              underline, strikeout,
                              color, background,
                              border_left, border_right,
                              latex_column_spec, latex_valign, include_thead,
                              link, image) {
  kable_attrs <- attributes(kable_input)
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
        x, width, border_left, border_right, latex_column_spec, latex_valign)
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

  if (table_info$duplicated_rows) {
    dup_fx_out <- fix_duplicated_rows_latex(out, table_info)
    out <- dup_fx_out[[1]]
    table_info <- dup_fx_out[[2]]
  }

  nrows <- length(table_info$contents)
  off <- table_info$position_offset

  bold <- ensure_len_latex(bold, nrows, off, include_thead, FALSE, "bold")
  italic <- ensure_len_latex(italic, nrows, off, include_thead, FALSE, "italic")
  monospace <- ensure_len_latex(monospace, nrows, off, include_thead, FALSE,
                             "monospace")
  underline <- ensure_len_latex(underline, nrows, off, include_thead, FALSE,
                             "underline")
  strikeout <- ensure_len_latex(strikeout, nrows, off, include_thead, FALSE,
                             "strikeout")
  color <- ensure_len_latex(color, nrows, off, include_thead, "black", "color")
  background <- ensure_len_latex(background, nrows, off, include_thead, "white",
                              "background")
  link <- ensure_len_latex(link, nrows, off, include_thead, "#", "link")
  image <- ensure_len_latex(image, nrows, off, include_thead, "", "image")

  if (include_thead) {
    rows <- seq(1, nrows)
  } else {
    rows <- seq(1 + off, nrows)
  }

  # issue #658: offset generates bad indices with single row tables
  rows <- intersect(rows, seq(nrows))

  for (i in rows) {
    target_row <- table_info$contents[i]
    new_row <- latex_cell_builder(
      target_row, column, table_info,
      bold[i], italic[i], monospace[i], underline[i],
      strikeout[i], color[i], background[i], link[i], image[i]
      # font_size, angle
      )
    temp_sub <- ifelse(i == 1 & (table_info$tabular == "longtable" |
                                   !is.null(table_info$repeat_header_latex)),
                       gsub, sub)
    out <- temp_sub(target_row, new_row, out, perl = T)
    table_info$contents[i] <- new_row
  }

  if (!is.null(width)) {
    if (is.null(table_info$column_width)) {
      table_info$column_width <- list()
    }
    for (i in column) {
      table_info$column_width[[paste0("column_", i)]] <- width
    }
  }
  out <- finalize_latex(out, kable_attrs, table_info)
  return(out)
}

ensure_len_latex <- function(x, l, off, include_thead, def, name) {
  if (is.null(x)) return(NULL)
  if (length(x) == 1) return(rep(x, l))
  if (include_thead) {
    if (length(x) == l) return(x)
    warning("The number of provided values in ", name, " does not equal to the ",
            "number of rows. ")
    return(rep(x, ceiling(l / length(x)))[seq(1, l)])
  } else {
    l_ = l - off
    if (length(x) == l_) return(c(def, x))
    warning("The number of provided values in ", name, " does not equal to the ",
            "number of rows. ")
    return(c(def, rep(x, ceiling(l_ / length(x)))[seq(1, l_)]))
  }
}

latex_column_align_builder <- function(x, width,
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
  # if (!is.null(color)) {
  #   color <- paste0("\\\\leavevmode\\\\color", latex_color(color))
  # }
  #
  # if (!is.null(background)) {
  #   background <- paste0("\\\\columncolor", latex_color(background))
  # }
  #
  # latex_array_options <- c("\\\\bfseries", "\\\\em", "\\\\ttfamily",
  #                          "\\\\underline", "\\\\sout")[
  #                            c(bold, italic, monospace, underline, strikeout)]
  # latex_array_options <- c(latex_array_options, extra_align,
  #                          color, background)
  latex_array_options <- paste0("\\>\\{", extra_align, "\\}")
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

latex_cell_builder <- function(target_row, column, table_info,
                               bold, italic, monospace,
                               underline, strikeout,
                               color, background, link, image
                               # font_size, angle
                               ) {
  new_row <- latex_row_cells(target_row)[[1]]
  if (bold) {
    new_row[column] <- paste0("\\\\textbf\\{", new_row[column], "\\}")
  }
  if (italic) {
    new_row[column] <- paste0("\\\\em\\{", new_row[column], "\\}")
  }
  if (monospace) {
    new_row[column] <- paste0("\\\\ttfamily\\{", new_row[column], "\\}")
  }
  if (underline) {
    new_row[column] <- paste0("\\\\underline\\{", new_row[column], "\\}")
  }
  if (strikeout) {
    new_row[column] <- paste0("\\\\sout\\{", new_row[column], "\\}")
  }
  if (!is.null(color)) {
    clean_columns <- unlist(lapply(new_row[column], clear_color_latex))
    new_row[column] <- paste0("\\\\textcolor", latex_color(color), "\\{",
                              clean_columns, "\\}")
  }
  # if (!is.null(font_size)) {
  #   new_row[column] <- paste0("\\\\begingroup\\\\fontsize\\{", font_size, "\\}\\{",
  #            as.numeric(font_size) + 2,
  #            "\\}\\\\selectfont ", new_row[column], "\\\\endgroup")
  # }
  # if (!is.null(angle)) {
  #   new_row[column] <- paste0("\\\\rotatebox\\{", angle, "\\}\\{",
  #                             new_row[column], "\\}")
  # }
  if (!is.null(background)) {
    clean_columns <- unlist(lapply(new_row[column], clear_color_latex, TRUE))
    new_row[column] <- paste0("\\\\cellcolor", latex_color(background), "\\{",
                              clean_columns, "\\}")
  }

  if (!is.null(link)) {
    new_row[column] <- paste0("\\\\href\\{", escape_latex(link), "\\}\\{",
                              new_row[column], "\\}")
  }

  if (!is.null(image) && (length(image) > 1 || !is.null(image[[1]]))) {
    image <- image[[1]]
    if (inherits(image, "kableExtraInlinePlots")) {
      new_row[column] <- paste0(
        new_row[column],
        '\\\\includegraphics\\[width=',
        # '\\\\raisebox\\{-\\\\totalheight\\}\\{\\\\includegraphics\\[width=',
        round(image$width / image$res, 2), 'in, height=',
        round(image$height / image$res, 2), 'in\\]\\{',
        image$path,
        '\\}'
        # '\\}\\}'
        )
    } else {
      if (!is.null(image) && !is.na(image) && image != "") {
        new_row[column] <- paste0(
          new_row[column],
          '\\\\includegraphics\\{',
          image, '\\}'
        )
      }
    }
  }

  new_row <- paste(new_row, collapse = " & ")

  return(new_row)
}
