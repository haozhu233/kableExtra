#' Add footnote (new)
#'
#' @description `footnote` provides a more flexible way to add footnote. You
#' can add multiple sets of footnote using different notation systems. It is
#' also possible to specify footnote section header one by one and print
#' footnotes as a chunk of texts.
#'
#' @param kable_input HTML or LaTeX table generated by `knitr::kable`
#' @param general Text for general footnote comments. Footnotes in this section
#' won't be labeled with any notations
#' @param number A vector of footnote texts. Footnotes here will be numbered.
#' There is no upper cap for the number of footnotes here
#' @param alphabet A vector of footnote texts, Footnotes here will be labeled
#' with "a,b,c". The vector here should not have more than 26 elements.
#' @param symbol A vector of footnote texts, Footnotes here will be labeled
#' with special symbols. The vector here should not have more than 20 elements.
#' @param footnote_order The order of how to arrange `general`, `number`,
#' `alphabet` and `symbol`.
#' @param footnote_as_chunk T/F value. Default is FALSE. It controls whether
#' the footnotes should be printed in a chunk (without line break).
#' @param escape T/F value. It controls whether the contents and titles should
#' be escaped against HTML or LaTeX. Default is TRUE.
#' @param threeparttable T/F value for whether to use LaTeX package
#' threeparttable. Threeparttable will force the width of caption and
#' footnotes be the width of the original table. It's useful when you have
#' long paragraph of footnotes.
#' @param fixed_small_size T/F When you want to keep the footnote small after
#' specifying large font size with the kable_styling() (e.g. ideal font for headers
#' and table content with small font in footnotes).
#' @param show_every_page T/F To make footnotes print at bottom of all pages for
#' long tables.  Only applies to pdf longtables.
#' @param general_title Section header for general footnotes. Default is
#' "Note: ".
#' @param number_title Section header for number footnotes. Default is "".
#' @param alphabet_title Section header for alphabet footnotes. Default is "".
#' @param symbol_title Section header for symbol footnotes. Default is "".
#' @param title_format Choose from "italic"(default), "bold" and "underline".
#' Multiple options are possible.
#' @param symbol_manual User can manually supply a vector of either html or
#' latex symbols. For example, `symbol_manual = c('*', '\\\\dag', '\\\\ddag')`.`
#'
#' @seealso [add_footnote()], [footnote_marker_number()]
#'
#' @examples
#' \dontrun{
#' dt <- mtcars[1:5, 1:5]
#' colnames(dt)[1] <- paste0("mpg",
#'                           footnote_marker_alphabet(2))
#' rownames(dt)[2] <- paste0(rownames(dt)[2],
#'                           footnote_marker_alphabet(1))
#' dt[1,2] <- paste0(dt[1,2], footnote_marker_alphabet(3))
#'
#' kbl(dt, escape = FALSE) |>
#'   footnote(alphabet = c("Note a", "Note b", "Note c"))
#' }
#'
#' @export
footnote <- function(kable_input,
                     general = NULL,
                     number = NULL,
                     alphabet = NULL,
                     symbol = NULL,
                     footnote_order = c("general", "number",
                                        "alphabet", "symbol"),
                     footnote_as_chunk = FALSE,
                     escape = TRUE,
                     threeparttable = FALSE,
                     fixed_small_size = FALSE,
                     show_every_page = FALSE,
                     general_title = "Note: ",
                     number_title = "",
                     alphabet_title = "",
                     symbol_title = "",
                     title_format = "italic",
                     symbol_manual = NULL
) {
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
  footnote_titles <- list(
    general = general_title, number = number_title,
    alphabet = alphabet_title, symbol = symbol_title
  )
  footnote_contents <- list(
    general = general, number = number, alphabet = alphabet, symbol = symbol
  )
  notnull <- names(footnote_contents)[!sapply(footnote_contents, is.null)]
  if (length(notnull) == 0) {return(kable_input)}
  footnote_order <- footnote_order[footnote_order %in% notnull]
  footnote_titles <- footnote_titles[footnote_order]
  footnote_contents <- footnote_contents[footnote_order]
  if (escape) {
    if (kable_format == "html") {
      footnote_contents <- lapply(footnote_contents, escape_html)
      footnote_titles <- lapply(footnote_titles, escape_html)
    } else {
      footnote_contents <- lapply(footnote_contents, escape_latex2)
      footnote_contents <- lapply(footnote_contents, linebreak)
      footnote_titles <- lapply(footnote_titles, escape_latex2)
      footnote_titles <- lapply(footnote_titles, linebreak)
    }
  }
  title_format <- match.arg(title_format, c("italic", "bold", "underline"),
                            several.ok = TRUE)
  footnote_titles <- lapply(footnote_titles, footnote_title_format,
                            kable_format, title_format)
  footnote_table <- footnote_table_maker(
    kable_format, footnote_titles, footnote_contents, symbol_manual
  )
  if (kable_format == "html") {
    return(footnote_html(kable_input, footnote_table, footnote_as_chunk))
  }
  if (kable_format == "latex") {
    return(footnote_latex(kable_input, footnote_table, footnote_as_chunk,
                          threeparttable, fixed_small_size, show_every_page))
  }
}

footnote_title_format <- function(x, format, title_format) {
  if (x == "") return(x)
  if (format == "html") {
    title_style <- ""
    if ("italic" %in% title_format) {
      title_style <- paste0(title_style, "font-style: italic;")
    }
    if ("bold" %in% title_format) {
      title_style <- paste0(title_style, "font-weight: bold;")
    }
    if ("underline" %in% title_format) {
      title_style <- paste0(title_style, "text-decoration: underline;")
    }
    return(paste0(
      '<span style="', title_style, '">', x, '</span>'
    ))
  } else {
    if ("italic" %in% title_format) {
      x <- paste0("\\\\textit\\{", x, "\\}")
    }
    if ("bold" %in% title_format) {
      x <- paste0("\\\\textbf\\{", x, "\\}")
    }
    if ("underline" %in% title_format) {
      x <- paste0("\\\\underline\\{", x, "\\}")
    }
    return(x)
  }
}

footnote_table_maker <- function(format, footnote_titles, footnote_contents,
                                 symbol_manual) {
  if (is.null(symbol_manual)) {
    number_index <- read.csv(system.file("symbol_index.csv",
                                         package = "kableExtra"))
    if (format == "latex") {
      symbol_index <- number_index$symbol.latex
    } else {
      symbol_index <- number_index$symbol.html
    }
  } else {
    symbol_index <- symbol_manual
  }


  if (!is.null(footnote_contents$general)) {
    footnote_contents$general <- data.frame(
      index = "",
      footnote = footnote_contents$general
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
footnote_html <- function(kable_input, footnote_table, footnote_as_chunk) {
  kable_attrs <- attributes(kable_input)
  important_nodes <- read_kable_as_xml(kable_input)
  body_node <- important_nodes$body
  kable_xml <- important_nodes$table

  new_html_footnote <- html_tfoot_maker(footnote_table, footnote_as_chunk)
  xml_add_child(kable_xml, new_html_footnote)
  xml2::xml_set_attr(kable_xml, "style",
                     paste0(xml2::xml_attr(kable_xml, "style"),
                            "border-bottom: 0;"))
  out <- as_kable_xml(body_node)
  attributes(out) <- kable_attrs
  if (!"kableExtra" %in% class(out)) class(out) <- c("kableExtra", class(out))
  return(out)
}

html_tfoot_maker <- function(footnote_table, footnote_as_chunk) {
  footnote_types <- names(footnote_table$contents)
  footnote_text <- c()
  for (i in footnote_types) {
    footnote_text <- c(footnote_text, html_tfoot_maker_(
      footnote_table$contents[[i]], footnote_table$titles[[i]], i,
      footnote_as_chunk))
  }
  footnote_text <- paste0(
    "<tfoot>", paste0(footnote_text, collapse = ""), "</tfoot>"
  )
  footnote_node <- read_html(footnote_text, options = c("RECOVER", "NOERROR"))
  return(xml_child(xml_child(footnote_node, 1), 1))
}

html_tfoot_maker_ <- function(ft_contents, ft_title, ft_type, ft_chunk) {
  footnote_text <- apply(ft_contents, 1, function(x) {
    paste0('<sup>', x[1], '</sup> ', x[2])
  })
  if (ft_title != "") {
    title_text <- ft_title
    footnote_text <- c(title_text, footnote_text)
  }
  if (!ft_chunk) {
    footnote_text <- paste0(
      '<tr><td style="padding: 0; " colspan="100%">',
      footnote_text, '</td></tr>'
    )
  } else {
    footnote_text <- paste0(
      '<tr><td style="padding: 0; " colspan="100%">',
      paste0(footnote_text, collapse = " "),
      '</td></tr>'
    )
  }
  return(footnote_text)
}

# LaTeX
footnote_latex <- function(kable_input, footnote_table, footnote_as_chunk,
                           threeparttable, fixed_small_size, show_every_page) {
  fn_regexp <- fn_text <- longtable_start <- longtable_text <- NULL

  kable_attrs <- attributes(kable_input)
  table_info <- magic_mirror(kable_input)
  out <- solve_enc(kable_input)
  fn_regexp <- fn_text <- longtable_start <- longtable_text <- NULL

  footnote_text <- latex_tfoot_maker(footnote_table, footnote_as_chunk,
                                     table_info$ncol, threeparttable)
  if (threeparttable) {
    if (table_info$tabular %in% c("longtable", "longtabu") ) {
      out <- sub(paste0("\\\\begin\\{", table_info$tabular, "\\}"),
                 paste0("\\\\begin{ThreePartTable}\n\\\\begin{TableNotes}",
                        ifelse(footnote_as_chunk, "[para]", ""),
                        ifelse(fixed_small_size,"\n\\\\small\n","\n"), footnote_text,
                        "\n\\\\end{TableNotes}\n\\\\begin{",
                        table_info$tabular, "}"),
                 out)
      out <- sub(paste0("\\\\end\\{",table_info$tabular, "\\}"),
                 paste0("\\\\end{", table_info$tabular,
                        "}\n\\\\end{ThreePartTable}"),
                 out)
      if (!show_every_page) {
        if (table_info$booktabs) {
          out <- sub(bottomrule_regexp,
                     "\\1\n\\\\insertTableNotes",
                     out)
        } else if (!show_every_page) {
          out <- sub("\\\\hline\n\\\\end\\{longtable\\}",
                     "\\\\hline\n\\\\insertTableNotes\n\\\\end\\{longtable\\}",
                     out)
        }
      }
    } else {
      if (table_info$tabular == "tabu") {
        stop("Please use `longtable = T` in your kable function. ",
             "Full width threeparttable only works with longtable.")
      }
      out <- sub(paste0("\\\\begin\\{", table_info$tabular, "\\}"),
                 paste0("\\\\begin{threeparttable}\n\\\\begin{",
                        table_info$tabular, "}"),
                 out)
      out <- sub(table_info$end_tabular,
                 paste0("\\\\end{", table_info$tabular,
                        "}\n\\\\begin{tablenotes}",
                        ifelse(footnote_as_chunk, "[para]", ""),
                        ifelse(fixed_small_size,"\n\\\\small\n","\n"), footnote_text,
                        "\n\\\\end{tablenotes}\n\\\\end{threeparttable}"),
                 out)
    }
  } else {
    if(!show_every_page) {
      if (table_info$booktabs) {
        out <- sub(bottomrule_regexp,
                   paste0("\\1\n", footnote_text),
                   out)
      } else {
        out <- sub(table_info$end_tabular,
                   paste0(footnote_text, "\n\\\\end{", table_info$tabular, "}"),
                   out)
      }
    }
  }

  if (table_info$tabular == "longtable" & show_every_page) {
    fn_regexp <- ifelse(threeparttable, "\\\\insertTableNotes",
                        footnote_text)
    fn_text <- gsub("\\\\", "\\", fn_regexp, fixed = TRUE)
    if(is.null(table_info$repeat_header_latex)) {
      # need full \begin{longtable} command
      # table_info valign2 ok but align missing vertical lines
      longtable_start <- sub(".*\\\\begin\\{longtable\\}",
                             "\\\\begin\\{longtable\\}", out)
      longtable_text <- sub("\n.*", "", longtable_start)
      out <- sub(longtable_text,
                 paste(longtable_text, fn_text, "\n\\endfoot\n"),
                 out, fixed = TRUE)
    } else {
      if(!table_info$booktabs){
        out <- sub(
          "\\\\endhead\\n",
          paste0("\\\\endhead\n",
                 fn_regexp, "\n\\\\endfoot\n",
                 fn_regexp, "\n\\\\endlastfoot\n"),
          out)
      } else {
        if (grepl(  # no repeat_header_continued in kable_styling
          "\\\\endhead\\n\\n\\\\endfoot",
          out)) {
          out <- sub(
            "\\\\endhead\\n\\n\\\\endfoot",
            paste0("\\\\endhead\n\\\\midrule\n", fn_regexp, "\n\\\\endfoot"),
            out)
        } else {  # repeat_header_continued in kable_styling
          out <- sub(
            "\\\\endfoot",
            paste0(fn_regexp, "\n\\\\endfoot"),
            out)
        }
        out <- sub(
          "\\\\endlastfoot",
          paste0(fn_regexp, "\n\\\\endlastfoot"),
          out)
      }
    }
  }

  out <- finalize_latex(out, kable_attrs, table_info)

  return(out)
}

latex_tfoot_maker <- function(footnote_table, footnote_as_chunk, ncol,
                              threeparttable) {
  footnote_types <- names(footnote_table$contents)
  footnote_text <- c()
  if (threeparttable) {
    for (i in footnote_types) {
      footnote_text <- c(footnote_text, latex_tfoot_maker_tpt_(
        footnote_table$contents[[i]], footnote_table$titles[[i]],
        footnote_as_chunk, ncol))
    }
  } else {
    for (i in footnote_types) {
      footnote_text <- c(footnote_text, latex_tfoot_maker_(
        footnote_table$contents[[i]], footnote_table$titles[[i]],
        footnote_as_chunk, ncol))
    }
  }
  footnote_text <- paste0(footnote_text, collapse = "\n")
  return(footnote_text)
}

latex_tfoot_maker_ <- function(ft_contents, ft_title, ft_chunk, ncol) {
  footnote_text <- apply(ft_contents, 1, function(x) {
    if (x[1] == "") {
      x[2]
    } else {
      paste0('\\\\textsuperscript{', x[1], '} ', x[2])
    }
  })
  if (ft_title != "") {
    title_text <- ft_title
    footnote_text <- c(title_text, footnote_text)
  }
  if (!ft_chunk) {
    footnote_text <- paste0(
      '\\\\multicolumn{', ncol, '}{l}{\\\\rule{0pt}{1em}', footnote_text, '}\\\\\\\\'
    )
  } else {
    footnote_text <- paste0(
      '\\\\multicolumn{', ncol, '}{l}{\\\\rule{0pt}{1em}',
      paste0(footnote_text, collapse = " "),
      '}\\\\\\\\'
    )
  }
  return(footnote_text)
}

latex_tfoot_maker_tpt_ <- function(ft_contents, ft_title, ft_chunk, ncol) {
  footnote_text <- apply(ft_contents, 1, function(x) {
    if (x[1] == "") {
      paste0('\\\\item ', x[2])
    } else {
      paste0('\\\\item[', x[1], '] ', x[2])
    }
  })
  if (ft_title != "") {
    title_text <- paste0('\\\\item ', ft_title, ' ')
    footnote_text <- c(title_text, footnote_text)
  }
  footnote_text <- paste0(footnote_text, collapse = "\n")
  # if (!ft_chunk) {
  #   footnote_text <- paste0(footnote_text, collapse = "\n")
  # } else {
  #   footnote_text <- paste0(footnote_text, collapse = " ")
  # }
  return(footnote_text)
}
