#' HTML table attributes
#'
#' @description This function provides a cleaner approach to modify the style
#' of HTML tables other than using the `table.attr` option in `knitr::kable()`. Note
#' that those bootstrap options requires Twitter bootstrap theme, which is not available
#' in some customized template being loaded.
#'
#' @param kable_input Output of `knitr::kable()` with `format` specified
#' @param bootstrap_options A character vector for bootstrap table options.
#' Please see package vignette or visit the w3schools'
#' \href{https://www.w3schools.com/bootstrap/bootstrap_tables.asp}{Bootstrap Page}
#' for more information. Possible options include `basic`, `striped`,
#' `bordered`, `hover`, `condensed`, `responsive` and `none`.
#' @param latex_options A character vector for LaTeX table options. Please see
#' package vignette for more information. Possible options include
#' `basic`, `striped`, `hold_position`, `HOLD_position`, `scale_down`, `scale_up` & `repeat_header`.
#' `striped` will add alternative row colors to the table. It will imports
#' `LaTeX` package `xcolor` if enabled. `hold_position` will "hold" the floating
#' table to the exact position. It is useful when the `LaTeX` table is contained
#'  in a `table` environment after you specified captions in `kable()`. It will
#'  force the table to stay in the position where it was created in the document.
#' A stronger version: `HOLD_position` requires the `float` package and specifies `[H]`.
#' `scale_down` is useful for super wide table. It will automatically adjust
#' the table to page width. `repeat_header` in only meaningful in a longtable
#' environment. It will let the header row repeat on every page in that long
#' table.
#' @param full_width A `TRUE` or `FALSE` variable controlling whether the HTML
#' table should have 100\% width. Since HTML and pdf have different flavors on
#' the preferable format for `full_width`. If not specified, a HTML table will
#' have full width by default but this option will be set to `FALSE` for a
#' LaTeX table
#' @param position A character string determining how to position the table
#' on a page. Possible values include `left`, `center`, `right`, `float_left`
#' and `float_right`. Please see the package doc site for demonstrations. For
#' a `LaTeX` table, if `float_*` is selected, `LaTeX` package `wrapfig` will be
#' imported.
#' @param font_size A numeric input for table font size
#' @param row_label_position A character string determining the justification
#' of the row labels in a table.  Possible values include `l` for left, `c` for
#' center, and `r` for right.  The default value is `l` for left justification.
#' @param repeat_header_text LaTeX option. A text string you want to append on
#' or replace the caption.
#' @param repeat_header_method LaTeX option, can either be `append`(default) or
#' `replace`
#' @param repeat_header_continued T/F or a text string. Whether or not to put
#' a continued mark on the second page of longtable. If you put in text, we will
#' use this text as the "continued" mark.
#' @param stripe_color LaTeX option allowing users to pick a different color
#' for their strip lines. This option is not available in HTML
#' @param stripe_index LaTeX option allowing users to customize which rows
#' should have stripe color.
#' @param latex_table_env LaTeX option. A character string to define customized
#' table environment such as tabu or tabularx.You shouldn't expect all features
#' could be supported in self-defined environments.
#' @param protect_latex If `TRUE`, LaTeX code embedded between dollar signs
#' will be protected from HTML escaping.
#' @param table.envir LaTeX floating table environment. `kable_style` will put
#' a plain no-caption table in a `table` environment in order to center the
#' table. You can specify this option to things like `table*` or `float*` based
#'  on your need.
#' @param fixed_thead HTML table option so table header row is fixed at top.
#' Values can be either T/F or `list(enabled = T/F, background = "anycolor")`.
#' @param htmltable_class Options to use the in-house lightable themes.
#' Choices include `lightable-minimal`, `lightable-classic`,
#' `lightable-classic-2`, `lightable-material`, `lightable-striped` and
#' `lightable-hover`. If you have your customized style sheet loaded which
#' defines your own table class, you can also load it here.
#' @param html_font A string for HTML css font. For example,
#' `html_font = '"Arial Narrow", arial, helvetica, sans-serif'`.
#' @param wraptable_width Width of the wraptable area if you specify
#' "float_left/right" for latex table. Default is "0pt" for automated
#' determination but you may specify it manually.
#'
#' @details  For LaTeX, if you use other than English environment
#' - all tables are converted to 'UTF-8'. If you use, for example, Hungarian
#' characters on a Windows machine, make sure to use
#' `Sys.setlocale("LC_ALL","Hungarian")` to avoid unexpected conversions.
#' - `protect_latex = TRUE` has no effect.
#'
#' For HTML,
#' - `protect_latex = TRUE` is for including complicated math in HTML output.
#' The LaTeX may not include dollar signs even if they are escaped.
#' Pandoc's rules for recognizing embedded LaTeX are used.
#'
#' @examples
#' \dontrun{
#' x_html <- knitr::kable(head(mtcars), "html")
#' kable_styling(x_html, "striped", position = "left", font_size = 7)
#'
#' x_latex <- knitr::kable(head(mtcars), "latex")
#' kable_styling(x_latex, latex_options = "striped", position = "float_left")
#' }
#'
#' @export
kable_styling <- function(kable_input,
                          bootstrap_options = "basic",
                          latex_options = "basic",
                          full_width = NULL,
                          position = "center",
                          font_size = NULL,
                          row_label_position = "l",
                          repeat_header_text = "\\textit{(continued)}",
                          repeat_header_method = c("append", "replace"),
                          repeat_header_continued = FALSE,
                          stripe_color = "gray!10",
                          stripe_index = NULL,
                          latex_table_env = NULL,
                          protect_latex = TRUE,
                          table.envir = "table",
                          fixed_thead = FALSE,
                          htmltable_class = NULL,
                          html_font = NULL,
                          wraptable_width = '0pt') {

  if (length(bootstrap_options) == 1 && bootstrap_options == "basic") {
    bootstrap_options <- getOption("kable_styling_bootstrap_options", "basic")
  }
  if (length(latex_options) == 1 && latex_options == "basic") {
    latex_options <- getOption("kable_styling_latex_options", "basic")
  }
  if (position == "center") {
    position <- getOption("kable_styling_position", "center")
  }
  position <- match.arg(position,
                        c("center", "left", "right", "float_left", "float_right"))
  if (is.null(font_size)) {
    font_size <- getOption("kable_styling_font_size", NULL)
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
    if (is.null(full_width)) {
      full_width <- getOption("kable_styling_full_width", T)
    }
    return(htmlTable_styling(kable_input,
                             bootstrap_options = bootstrap_options,
                             full_width = full_width,
                             position = position,
                             font_size = font_size,
                             protect_latex = protect_latex,
                             fixed_thead = fixed_thead,
                             htmltable_class = htmltable_class,
                             html_font = html_font))
  }
  if (kable_format == "latex") {
    if (is.null(full_width)) {
      full_width <- getOption("kable_styling_full_width", F)
    }
    repeat_header_method <- match.arg(repeat_header_method)
    return(pdfTable_styling(kable_input,
                            latex_options = latex_options,
                            full_width = full_width,
                            position = position,
                            font_size = font_size,
                            row_label_position = row_label_position,
                            repeat_header_text = repeat_header_text,
                            repeat_header_method = repeat_header_method,
                            repeat_header_continued = repeat_header_continued,
                            stripe_color = stripe_color,
                            stripe_index = stripe_index,
                            latex_table_env = latex_table_env,
                            table.envir = table.envir,
                            wraptable_width = wraptable_width))
  }
}

extract_latex_from_kable <- function(kable_input) {
  kable_attrs <- attributes(kable_input)
  regexp <- paste0("(?<!\\e)",   # Not escaped
                   "([$]{1}(?![ ])[^$]+(?<![$\\\\ ])[$]{1}", # $...$
                   "|[$]{2}(?![ ])[^$]+(?<![$\\\\ ])[$]{2})", # $$...$$
                   "(?!\\d)")        # Not followed by digit
  latex <- character()
  while (str_detect(kable_input, regexp)) {
    block <- str_extract(kable_input, regexp)
    name <- paste0("latex", digest(block))
    latex[name] <- block
    kable_input <- str_replace(kable_input, regexp, name)
  }
  kable_attrs$extracted_latex <- latex
  attributes(kable_input) <- kable_attrs
  kable_input
}

replace_latex_in_kable <- function(kable_input, latex) {
  kable_attrs <- attributes(kable_input)
  for (n in names(latex)) {
    kable_input <- str_replace_all(kable_input, fixed(n), latex[n])
  }
  attributes(kable_input) <- kable_attrs
  kable_input
}

# htmlTable Styling ------------
htmlTable_styling <- function(kable_input,
                              bootstrap_options = "basic",
                              full_width = T,
                              position = c("center", "left", "right",
                                           "float_left", "float_right"),
                              font_size = NULL,
                              protect_latex = TRUE,
                              fixed_thead = FALSE,
                              htmltable_class = NULL,
                              html_font = NULL) {
  if (protect_latex) {
    kable_input <- extract_latex_from_kable(kable_input)
  }
  kable_attrs <- attributes(kable_input)
  important_nodes <- read_kable_as_xml(kable_input)
  body_node <- important_nodes$body
  kable_xml <- important_nodes$table

  # Modify class
  bootstrap_options <- match.arg(
    bootstrap_options,
    c("basic", "striped", "bordered", "hover", "condensed", "responsive",
      "none"),
    several.ok = T
  )

  kable_xml_class <- NULL
  if (xml_has_attr(kable_xml, "class")) {
    kable_xml_class <- xml_attr(kable_xml, "class")
  }

  if (!is.null(htmltable_class)) {
    bootstrap_options <- "none"
    xml_attr(kable_xml, "class") <- paste(kable_xml_class, htmltable_class)
  }

  if (length(bootstrap_options) == 1 && bootstrap_options == "none") {
  }else {
    if (length(bootstrap_options) == 1 && bootstrap_options == "basic") {
      bootstrap_options <- "table"
    } else {
      bootstrap_options <- bootstrap_options[bootstrap_options != "basic"]
      bootstrap_options <- paste0("table-", bootstrap_options)
      bootstrap_options <- c("table", bootstrap_options)
    }
    xml_attr(kable_xml, "class") <- paste(c(kable_xml_class, bootstrap_options),
                                          collapse = " ")
  }

  # Modify style
  kable_xml_style <- NULL
  if (xml_has_attr(kable_xml, "style")) {
    kable_xml_style <- xml_attr(kable_xml, "style")
  }
  if (!is.null(font_size)) {
    if (is.numeric(font_size)) font_size <- paste0(font_size, "px")
    kable_xml_style <- c(kable_xml_style,
                         paste0("font-size: ", font_size, ";"))
    kable_caption_node <- xml_tpart(kable_xml, "caption")
    if (!is.null(kable_caption_node)) {
      xml_attr(kable_caption_node, "style") <- "font-size: initial !important;"
    }
  }

  # issue 689: invisible font in Rstudio dark theme
  flag <- tryCatch(rstudioapi::getThemeInfo()$dark, error = function(e) FALSE)
  if (isTRUE(flag)) {
    kable_xml_style <- c(kable_xml_style, "color: black;")
  }

  if (!is.null(html_font)) {
    kable_xml_style <- c(kable_xml_style, paste0(
      'font-family: ', html_font, ';'
    ))
  }
  if (!full_width) {
    kable_xml_style <- c(kable_xml_style, "width: auto !important;")
  }

  position <- match.arg(position)
  position_style <- switch(
    position,
    center = "margin-left: auto; margin-right: auto;",
    left = "",
    right = "margin-right: 0; margin-left: auto",
    float_left = "float: left; margin-right: 10px;",
    float_right = "float: right; margin-left: 10px;"
  )
  kable_xml_style <- c(kable_xml_style, position_style)

  if (length(kable_xml_style) != 0) {
    xml_attr(kable_xml, "style") <- paste(kable_xml_style, collapse = " ")
  }

  fixed_thead <- get_fixed_thead(fixed_thead)
  if (fixed_thead$enabled) {
    all_header_cells <- xml2::xml_find_all(kable_xml, "//thead//th")
    if (is.null(fixed_thead$background))  fixed_thead$background <- "#FFFFFF"
    for (i in seq(length(all_header_cells))) {
      xml_attr(all_header_cells[i], "style") <- paste0(
        xml_attr(all_header_cells[i], "style"),
        "position: sticky; top:0; background-color: ",
        fixed_thead$background, ";"
      )
    }
  }

  out <- as_kable_xml(body_node)
  if (protect_latex) {
    out <- replace_latex_in_kable(out, kable_attrs$extracted_latex)
    kable_attrs$extracted_latex <- NULL
  }
  attributes(out) <- kable_attrs
  if (!"kableExtra" %in% class(out)) class(out) <- c("kableExtra", class(out))
  return(out)
}

# LaTeX table style ------------
pdfTable_styling <- function(kable_input,
                             latex_options = "basic",
                             full_width = FALSE,
                             position,
                             font_size,
                             row_label_position,
                             repeat_header_text,
                             repeat_header_method,
                             repeat_header_continued,
                             stripe_color,
                             stripe_index,
                             latex_table_env,
                             table.envir,
                             wraptable_width) {
  kable_attrs <- attributes(kable_input)
  latex_options <- match.arg(
    latex_options,
    c("basic", "striped", "hold_position", "HOLD_position", "scale_down", "scale_up", "repeat_header"),
    several.ok = T
  )

  out <- NULL
  out <- solve_enc(kable_input)

  table_info <- magic_mirror(kable_input)

  if ("striped" %in% latex_options) {
    out <- styling_latex_striped(out, table_info, stripe_color, stripe_index)
    table_info <- magic_mirror(out)
  }

  # hold_position is only meaningful in a table environment
  if ("hold_position" %in% latex_options & table_info$table_env) {
    out <- styling_latex_hold_position(out)
  }

  # HOLD_position is only meaningful in a table environment
  if ("HOLD_position" %in% latex_options & table_info$table_env) {
    out <- styling_latex_HOLD_position(out)
  }

  if ("scale_down" %in% latex_options) {
    out <- styling_latex_scale(out, table_info, "down")
  }

  if ("scale_up" %in% latex_options) {
    out <- styling_latex_scale(out, table_info, "up")
  }

  if ("repeat_header" %in% latex_options & table_info$tabular == "longtable") {
    out <- styling_latex_repeat_header(out, table_info, repeat_header_text,
                                       repeat_header_method, repeat_header_continued)
    table_info$repeat_header_latex <- TRUE
  }

  if (full_width) {
    latex_table_env <- ifelse(table_info$tabular == "longtable",
                              "longtabu", "tabu")
    full_width_return <- styling_latex_full_width(out, table_info)
    out <- full_width_return[[1]]
    table_info$align_vector <- full_width_return[[2]]
  }

  if (!is.null(font_size)) {
    out <- styling_latex_font_size(out, table_info, font_size)
  }

  if (!is.null(latex_table_env)) {
    out <- styling_latex_table_env(out, table_info$tabular, latex_table_env)
    table_info$tabular <- latex_table_env
    table_info$begin_tabular <- sub("tabular", latex_table_env,
                                    table_info$begin_tabular)
    table_info$end_tabular <- sub("tabular", latex_table_env,
                                  table_info$end_tabular)
  }

  out <- styling_latex_position(out, table_info, position, latex_options,
                                table.envir, wraptable_width)

  out <- finalize_latex(out, kable_attrs, table_info)

  if (row_label_position != "l") {
    if (table_info$tabular == "longtable") {
      out <- sub("\\\\begin\\{longtable\\}\\{l",
                 paste0("\\\\begin\\{longtable\\}\\{",
                        row_label_position),
                 out)
    } else {
      out <- sub("\\\\begin\\{tabular\\}\\{l",
                 paste0("\\\\begin\\{tabular\\}\\{",
                        row_label_position),
                 out)
    }
  }

  return(out)
}

styling_latex_striped <- function(x, table_info, color, stripe_index) {
  if (is.null(stripe_index)) {
    stripe_index <- seq(
      1,
      # Issue #613
      max(1, table_info$nrow - table_info$position_offset),
      2)
  }
  row_spec(x, stripe_index, background = color)
}

styling_latex_hold_position <- function(x) {
  if (str_detect(x, "\\\\begin\\{table\\}\\[t\\]")) {
    str_replace(x, "\\\\begin\\{table\\}\\[t\\]", "\\\\begin\\{table\\}[!h]")
  } else {
    str_replace(x, "\\\\begin\\{table\\}", "\\\\begin\\{table\\}[!h]")
  }
}

styling_latex_HOLD_position <- function(x) {
  if (str_detect(x, "\\\\begin\\{table\\}\\[t\\]")) {
    str_replace(x, "\\\\begin\\{table\\}\\[t\\]", "\\\\begin\\{table\\}[H]")
  } else {
    str_replace(x, "\\\\begin\\{table\\}", "\\\\begin\\{table\\}[H]")
  }
}

styling_latex_scale <- function(x, table_info, dir=c("down", "up")) {
  # You cannot put longtable in a resizebox
  # http://tex.stackexchange.com/questions/83457/how-to-resize-or-scale-a-longtable-revised
  if (table_info$tabular == "longtable") {
    warning("Longtable cannot be resized.")
    return(x)
  }
  if (dir=="down") {
    d <- ">"
  } else {
    d <- "<"
  }

  x <- sub(table_info$begin_tabular,
           paste0("\\\\resizebox{\\\\ifdim\\\\width\\", d,"\\\\linewidth\\\\linewidth\\\\else\\\\width\\\\fi\\}\\{\\!\\}\\{\n",
		  table_info$begin_tabular),
           x)
  sub(table_info$end_tabular, paste0(table_info$end_tabular, "\\}"), x)
}

styling_latex_repeat_header <- function(x, table_info, repeat_header_text,
                                        repeat_header_method,
                                        repeat_header_continued) {
  x <- str_split(x, "\n")[[1]]
  # These two defs won't be used, but make it clear
  # the rules are defined
  midrule <- "\\midrule"
  bottomrule <- "\\bottomrule"
  #
  if (table_info$booktabs) {
    header_rows_start <- grep(paste0("^", toprule_regexp, "$"),  x)[1]
    if (is.null(table_info$colnames)) {
      header_rows_end <- header_rows_start
    } else {
      header_rows_end <- grep(paste0("^", midrule_regexp, "$"), x)[1]
      midrule <- sub(midrule_regexp, "\\1", x[header_rows_end])
    }
  } else {
    header_rows_start <- which(x == "\\hline")[1]
    header_rows_end <- which(x == "\\hline")[2]
  }

  if (is.na(table_info$caption)) {
    continue_line <- paste0(
      "\\multicolumn{", table_info$ncol, "}{@{}l}{", repeat_header_text,
      "}\\\\"
    )
  } else {
    if (repeat_header_method == "append") {
      caption_without_lab <- sub("\\\\label\\{[^\\}]*\\}", "", table_info$caption)
      repeat_header_text <- paste(caption_without_lab, repeat_header_text)
    }
    continue_line <- paste0("\\caption[]{", repeat_header_text, "}\\\\")
  }

  if (!table_info$booktabs) {
    bottom_part <- NULL
  } else {
    index_bottomrule <- grep(paste0("^", bottomrule_regexp, "$"), x)
    bottomrule <- x[index_bottomrule]
    x <- x[-index_bottomrule]
    x[index_bottomrule - 1] <- paste0(x[index_bottomrule - 1], "*")

    if (repeat_header_continued == FALSE) {
      bottom_part <- paste0( "\n\\endfoot\n", bottomrule,
                             "\n\\endlastfoot")
    } else {
      if (repeat_header_continued == TRUE) {
        bottom_text <- "\\textit{(continued \\ldots)}"
      } else {
        bottom_text <- repeat_header_continued
      }
      bottom_part <- paste0(
        midrule, "\n",
        "\\multicolumn{", table_info$ncol, "}{r@{}}{", bottom_text, "}\\\\\n",
        "\\endfoot\n",
        bottomrule, "\n",
        "\\endlastfoot"
      )
    }
  }

  # x[index_bottomrule - 1] <- paste0(x[index_bottomrule - 1], "*\\bottomrule")
  x <- c(
    x[1:header_rows_end],
    "\\endfirsthead",
    continue_line,
    x[header_rows_start:header_rows_end],
    "\\endhead",
    bottom_part,
    x[(header_rows_end + 1):length(x)]
  )
  x <- paste0(x, collapse = "\n")
  return(x)
}

styling_latex_full_width <- function(x, table_info) {
  col_align <- as.character(factor(
    table_info$align_vector, c("c", "l", "r"),
    c(">{\\\\centering}X", ">{\\\\raggedright}X", ">{\\\\raggedleft}X")
  ))
  col_align[is.na(col_align)] <- table_info$align_vector[is.na(col_align)]
  col_align_vector <- col_align
  col_align <- paste0(" to \\\\linewidth {", paste(col_align, collapse = ""), "}")
  x <- sub(paste0(table_info$begin_tabular, "\\{[^\\\\n]*\\}"),
           table_info$begin_tabular, x)
  x <- sub(table_info$begin_tabular,
      paste0(table_info$begin_tabular, col_align), x)
  return(list(x, col_align_vector))
}

styling_latex_position <- function(x, table_info, position, latex_options,
                                   table.envir, wraptable_position) {
  hold_position <- intersect(c("hold_position", "HOLD_position"), latex_options)
  if (length(hold_position) == 0) hold_position <- ""
  switch(
    position,
    center = styling_latex_position_center(x, table_info, hold_position,
                                           table.envir),
    left = styling_latex_position_left(x, table_info),
    right = styling_latex_position_right(x, table_info, hold_position,
                                         table.envir),
    float_left = styling_latex_position_float(x, table_info, "l", table.envir,
                                              wraptable_position),
    float_right = styling_latex_position_float(x, table_info, "r", table.envir,
                                               wraptable_position)
  )
}

styling_latex_position_center <- function(x, table_info, hold_position,
                                          table.envir) {
  if (!table_info$table_env && table_info$tabular == "tabular") {
    x <- paste0("\\begin{", table.envir, "}\n\\centering", x,
                "\n\\end{", table.envir, "}")
    if (hold_position == "hold_position") {
      x <- styling_latex_hold_position(x)
    } else if(hold_position == "HOLD_position") {
      x <- styling_latex_HOLD_position(x)
    }
  } else if (table_info$table_env) {
    x <- sub("^(\\\\begin\\{table}[^\n]*)\\n", "\\1\n\\\\centering", x)
  }
  return(x)
}

styling_latex_position_left <- function(x, table_info) {
  if (table_info$tabular != "longtable") return(sub("\\\\centering\\n", "", x))
  longtable_option <- "\\[l\\]"
  sub(paste0("\\\\begin\\{longtable\\}", table_info$valign2),
      paste0("\\\\begin\\{longtable\\}", longtable_option), x)
}

styling_latex_position_right <- function(x, table_info, hold_position,
                                         table.envir) {
  warning("Position = right is only supported for longtable in LaTeX. ",
          "Setting back to center...")
  styling_latex_position_center(x, table_info, hold_position, table.envir)
}

styling_latex_position_float <- function(x, table_info, option, table.envir,
                                         wraptable_width) {
  if (table_info$tabular == "longtable") {
    warning("wraptable is not supported for longtable.")
    if (option == "l") return(styling_latex_position_left(x, table_info))
    if (option == "r") return(styling_latex_position_right(x, table_info, F,
                                                           table.envir))
  }
  size_matrix <- sapply(sapply(table_info$contents, str_split, " & "), nchar)
  col_max_length <- apply(size_matrix, 1, max) + 4
  if (table_info$table_env) {
    option <- sprintf("\\\\begin\\{wraptable\\}\\{%s\\}", option)
    option <- paste0(option, "\\{", wraptable_width, "\\}")
    x <- sub("\\\\begin\\{table\\}\\[\\!h\\]", "\\\\begin\\{table\\}", x)
    x <- sub("\\\\begin\\{table\\}", option, x)
    x <- sub("\\\\end\\{table\\}", "\\\\end\\{wraptable\\}", x)
  } else {
    option <- sprintf("\\begin{wraptable}{%s}", option)
    option <- paste0(option, "{0pt}")
    x <- paste0(option, x, "\\end{wraptable}")
  }
  return(x)
}

styling_latex_font_size <- function(x, table_info, font_size) {
  row_height <- font_size + 2
  if (table_info$tabular != "longtable" & table_info$table_env) {
    return(sub(table_info$begin_tabular,
               paste0("\\\\fontsize\\{", font_size, "\\}\\{", row_height,
                      "\\}\\\\selectfont\n", table_info$begin_tabular),
               x))
  }
  # For longtable and tabular without table environment. Simple wrap around
  # fontsize is good enough
  return(paste0(
    "\\begingroup\\fontsize{", font_size, "}{", row_height, "}\\selectfont\n", x,
    "\n\\endgroup{}"
  ))
}

styling_latex_table_env <- function(x, current_env, latex_table_env) {
  x <- sub(
    paste0("begin\\{", current_env, "\\}\\[t\\]"),
    paste0("begin\\{", latex_table_env, "\\}"), x
  )
  x <- sub(
    paste0("begin\\{", current_env, "\\}"),
    paste0("begin\\{", latex_table_env, "\\}"), x
  )
  x <- sub(
    paste0("end\\{", current_env, "\\}"),
    paste0("end\\{", latex_table_env, "\\}"), x
  )
  return(x)
}

