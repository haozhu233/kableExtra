#' Add footnote
#'
#' @description Add footnote to your favorite kable output.
#'
#' @param input The direct output of your \code{kable} function or your last
#' \code{kableExtra} function.
#' @param label A vector of footnotes you want to add. You don't need to add
#' notations in your notes.
#' @param notation You can select the format of your footnote notation from
#' `number`, `alphabet`, `symbol` and `none`.
#' @param threeparttable Boolean value indicating if a
#' \href{https://www.ctan.org/pkg/threeparttable}{threeparttable} scheme should
#' be used.
#' @param escape Logical value controlling if the label needs to be escaped.
#' Default is TRUE.
#'
#' @seealso [footnote()], [footnote_marker_number()]
#' @examples
#' \dontrun{
#' x <- knitr::kable(head(mtcars), "html")
#' add_footnote(x, c("footnote 1", "footnote 2"), notation = "symbol")
#' }
#'
#' @export
add_footnote <- function(input, label = NULL,
                         notation = getOption("kable_footnote_notation", "alphabet"),
                         threeparttable = FALSE,
                         escape = TRUE) {
  if (is.null(label)) return(input)

  if (!threeparttable) {
    threeparttable <- getOption("kable_footnote_threeparttable", FALSE)
  }

  table_info <- NULL

  notation <- match.arg(notation, c("alphabet", "number", "symbol", "none"))
  if (notation == "none") {
    ids <- rep("", 20)
    ids.intable <- ids
  } else {
    if (notation == "symbol") {
      notation <- paste0(notation, ".", attr(input, "format"))
    }

    ids.ops <- read.csv(system.file("symbol_index.csv", package = "kableExtra"))
    ids <- ids.ops[, notation]
    ids.intable <- gsub("\\*", "\\\\*", ids)
  }


  #count the number of items in label and intable notation
  count.label <- length(label)
  count.intablenote <- sum(str_count(input, "\\[note\\]"))
  if (count.intablenote != 0 & count.label != count.intablenote) {
    warning(paste("You entered", count.label, "labels but you put",
                  count.intablenote, "[note] in your table."))
  }

  export <- input

  # Find out if there are any extra in-table notations needed to be corrected
  extra.notation <- unique(as.numeric(
    str_extract(
      str_extract_all(
        paste0(export, collapse = ""), "\\[note[0-9]{1,2}\\]"
      )[[1]],
      "[0-9]{1,2}")))

  # Pandoc ---------------------------
  # Footnote solution for markdown and Pandoc. It is not perfect as
  # markdown doesn't support complex table formats but this solution
  # should be able to satisfy people who don't want to spend extra
  # time to define their `kable` format.
  if (!attr(input, "format") %in% c("html", "latex")) {
    if (notation == "none")
      ids.innote <- ids.intable  # issue #672
    else
      ids.innote <- paste0("^", ids.intable, "^")
    # In table notation
    if (count.intablenote != 0) {
      for (i in 1:count.intablenote) {
        replace_note <- paste0(ids.innote[i],
                               paste0(rep(" ", 4 - ceiling(i/5)), collapse = ""))

        export[which(str_detect(export, "\\[note\\]"))[1]] <-
          sub("\\[note\\]", replace_note,
              export[which(str_detect(export, "\\[note\\]"))[1]])
      }
    }
    # Fix extra in table notation
    for (i in extra.notation) {
      export <- gsub(paste0("\\[note", i, "\\]"),
                     paste0(ids.innote[i],
                            paste0(rep(" ", 4 - ceiling(i/5)), collapse = "")),
                     export)
    }

    export[length(export) + 1] <- ""
    export[length(export) + 1] <- "__Note:__"
    export[length(export) + 1] <- paste0(
      paste0(ids.innote[1:length(label)], label), collapse = " "
    )
  }

  # LaTeX Tables --------------------------------
  if (attr(input, "format") == "latex") {
    # Clean the entry for labels
    if (escape) {
      label <- escape_latex(label)
      label <- linebreak(label)
    }
    label <- gsub("\\\\", "\\\\\\\\", label)

    export <- enc2utf8(export)
    table_info <- magic_mirror(input)
    if (table_info$tabular == "longtable") {
      if (notation != "number") {
        warning("Notation is set to 'number' and other formats are not supported.")
        notation <- "number"
      }
      # If longtable is used, then use page footnote instead of threeparttable
      # as it makes more sense to see the footnote at the bottom of page if
      # table is longer than one page.
      if (threeparttable == T) {
        warning("Threeparttable does not support longtable.")
        threeparttable = F
      }

      # Longtable doesn't support footnote in caption directly.
      # See http://tex.stackexchange.com/questions/50151/footnotes-in-longtable-captions

      count.in.caption.note <- 0
      if (!is.na(table_info$caption)) {
        count.in.caption.note <- str_count(table_info$caption, "\\[note\\]")
      }
      if (count.in.caption.note != 0) {
        caption.footnote <- paste0("\\\\addtocounter{footnote}{-",
                                   count.in.caption.note, "}")
        for (i in 1:count.in.caption.note) {
          export <- sub("\\[note\\]", "\\\\protect\\\\footnotemark ", export)
          caption.footnote <- paste0(
            caption.footnote, "\n\\\\stepcounter{footnote}\\\\footnotetext{",
            label[i], "}")
        }

        if (str_detect(export, toprule_regexp)) {
          export <- sub(toprule_regexp,
                        paste0("\\1\n", caption.footnote), export)
        } else {
          export <- sub("\\\\hline",
                        paste0("\\\\hline\n", caption.footnote), export)
        }
      }
      for (i in (count.in.caption.note + 1):count.intablenote) {
        export <- sub("\\[note\\]",
                      paste0("\\\\footnote[", i, "]{", label[i], "}"), export)
      }
      for (i in extra.notation) {
        export <- gsub(paste0("\\[note", i, "\\]"),
                       paste0("\\\\footnotemark[", i, "]"),
                       export)
      }
    } else {
      # Replace in-table notation with appropriate symbol
      for (i in 1:count.intablenote) {
        export <- sub("\\[note\\]",
                      paste0("\\\\textsuperscript{", ids.intable[i], "}"),
                      export)
      }

      # Fix extra in table notation
      for (i in extra.notation) {
        export <- gsub(paste0("\\[note", i, "\\]"),
                       paste0("\\\\textsuperscript{", ids.intable[i], "}"),
                       export)
      }
      if (threeparttable == T) {
        # generate footer with appropriate symbol
        usepackage_latex("threeparttable")
        footer <- ""
        for (i in 1:count.label) {
          footer <- paste0(footer,"\\\\item [", ids[i], "] ", label[i], "\n")
        }

        if (grepl("\\\\caption\\{.*?\\}", export)) {
          export <- sub("\\\\caption\\{",
                        "\\\\begin{threeparttable}\n\\\\caption{",
                        export)
        } else {
          export <- sub(paste0("\\\\begin\\{", table_info$tabular, "\\}"),
                        paste0("\\\\begin{threeparttable}\n\\\\begin{",
                               table_info$tabular, "}"),
                        export)
        }
        export <- gsub(
          "\\\\end\\{tabular\\}",
          paste0("\\\\end{tabular}\n\\\\begin{tablenotes}\n\\\\small\n",
                 footer, "\\\\end{tablenotes}\n\\\\end{threeparttable}"),
          export)
      } else {
        table.width <- max(nchar(
          str_replace_all(
            str_replace_all(table_info$contents, "\\[note\\]", ""),
            "\\[note[0-9]{1,2}\\]", ""))) + 2 * (table_info$ncol - 1)
        footer <- ""
        for (i in 1:count.label) {
          label.wrap <- strwrap(label[i], table.width)
          footer <- paste0(footer, "\\\\multicolumn{", table_info$ncol,
                           "}{l}{\\\\textsuperscript{", ids[i], "} ",
                           label.wrap[1], "}\\\\\\\\\n")
          if (length(label.wrap) > 1) {
            for (j in 2:length(label.wrap)) {
              footer <- paste0(footer, "\\\\multicolumn{", table_info$ncol,
                               "}{l}{", label.wrap[j], "}\\\\\\\\\n")
            }
          }
        }
        export <- gsub(table_info$end_tabular,
                       paste0(footer, "\\\\end{", table_info$tabular, "}"),
                       export)
      }
    }
  }

  # HTML Tables -------------------
  if (attr(input, "format") == "html") {
    # Clean the entry for labels
    table_info <- magic_mirror(input)
    if (escape) {
      label <- escape_html(label)
    }
    # Replace in-table notation with appropriate symbol
    for (i in 1:count.intablenote) {
      export <- sub("\\[note\\]",
                    paste0("<sup>", ids.intable[i], "</sup>"),
                    export)
    }

    # Fix extra in table notation
    for (i in extra.notation) {
      export <- gsub(paste0("\\[note", i, "\\]"),
                     paste0("<sup>", ids.intable[i], "</sup>"),
                     export)
    }

    # Build footer
    footer <- "<tfoot>\n"
    for (i in 1:count.label) {
      footer <- paste0(footer, "<tr>\n<td style = 'padding: 0; border:0;' ",
                       "colspan='100%'><sup>", ids[i],"</sup> ",
                       label[i], "</td>\n</tr>\n")
    }
    footer <- paste0(footer, "</tfoot>\n")

    # Paste footer to the table
    export[1] <- gsub("</tbody>\n", paste0("</tbody>\n", footer), export[1])
  }
  attr(export, "kable_meta") <- table_info
  return(export)
}
