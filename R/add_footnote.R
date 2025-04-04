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

  format <- attr(input, "format")

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
      # FIXME:  temporary kludge
      if (format == "latex")
        notation <- paste0(notation, "2")
    }

    ids.ops <- read.csv(system.file("symbol_index.csv", package = "kableExtra2"))
    ids <- ids.ops[, notation]
    ids.intable <- gsub("\\*", "\\\\*", ids)
  }

  if (format == "latex") {
    parsed <- kable_to_parsed(input)
    res <- add_footnote_latex(parsed, label,
                              notation,
                              ids,
                              threeparttable,
                              escape)
    return(parsed_to_kable(res, input))
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
  if (!format %in% c("html", "latex")) {
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


  # HTML Tables -------------------
  if (format == "html") {
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

replace_note_latex <- function(parsed, note_location, ids) {
  for (i in rev(seq_along(note_location))) {
    note <- get_range(parsed, note_location[[i]])
    if (length(find_pattern(note, "[0-9]", all = FALSE))) {
      digits <- find_catcode(note, "SPECIAL")
      value <- 0
      for (j in seq_along(digits)) {
        chars <- note[[digits[j]]]
        if (grepl("[0-9]", chars))
          value <- 10*value + as.numeric(chars)
      }
    } else
      value <- i
    parsed <- set_range(parsed, note_location[[i]],
                        latex2("\\textsuperscript",
                                       new_block(latex2(ids[value]))))
  }
  parsed
}

add_footnote_latex <- function(parsed, label,
                               notation,
                               ids,
                               threeparttable,
                               escape) {

  #count the number of items in label and intable notation
  count.label <- length(label)
  notes <- find_pattern(parsed, "\\[note[0-9]{0,2}\\]", all = TRUE)
  count.notes <- length(notes)
  if (count.notes != 0 && count.label != count.notes) {
    warning(paste("You entered", count.label, "labels but you put",
                  count.notes, "[note] markers in your table."))
  }

  # Clean the entry for labels
  if (escape) {
    label <- escape_latex(label)
    label <- linebreak(label)
  }
  # Labels remain unparsed for now...

  table_info <- magic_mirror(parsed)

  if (table_info$tabular == "longtable") {
    if (notation != "number") {
      warning("Notation is set to 'number' and other formats are not supported.")
      notation <- "number"
    }
    # If longtable is used, then use page footnote instead of threeparttable
    # as it makes more sense to see the footnote at the bottom of page if
    # table is longer than one page.
    if (threeparttable) {
      warning("Threeparttable does not support longtable.")
      threeparttable <- FALSE
    }

    # Longtable doesn't support footnote in caption directly.
    # See http://tex.stackexchange.com/questions/50151/footnotes-in-longtable-captions

    count.in.caption.note <- 0
    if (length(table_info$captionPath)) {
      caption_notes <- find_pattern(table_info$caption,
                                    "\\[note[0-9]{0,2}\\]", all = TRUE)

      count.in.caption.note <- length(caption_notes)
    }
    if (count.in.caption.note != 0) {
      caption.footnote <- latex2(paste0("\\addtocounter{footnote}{-",
                                        count.in.caption.note, "}"))
      caption <- replace_note_latex(table_info$caption,
                                    caption_notes, ids)

      for (i in seq_len(count.in.caption.note)) {
        caption.footnote <- insert_values(caption.footnote, length(caption.footnote), after = TRUE,
                                          latex2(
                                            "\n\\stepcounter{footnote}\\footnotetext", new_block(
                                              label[i])))
      }
      parsed[[table_info$captionPath]] <-
        table_info$caption <- caption

      rule <- find_macro(table, "\\toprule", all = FALSE)
      if (!length(rule))
        rule <- find_macro(table, "\\hline", all = FALSE)
      if (length(rule))
        table <- insert_values(table, rule, after = TRUE,
                               latex2("\\1\n", caption.footnote))
    }
    parsed <- replace_note_latex(parsed, notes, ids)
  } else {
    # Replace in-table notation with appropriate symbol
    parsed <- replace_note_latex(parsed, notes, ids)

    table_info$tabularPath <- getTabularPath(parsed)
    table <- parsed[[table_info$tabularPath]]
    if (threeparttable) {
      # generate footer with appropriate symbol
      usepackage_latex("threeparttable")
      footer <- ""
      for (i in 1:count.label) {
        footer <- paste0(footer,"\\item  [", ids[i], "] ", label[i], "\n")
      }

      if (length(table_info$tablePath)) {
        table <- parsed[[table_info$tablePath]]
        contents <- get_contents(table)
        threepart <- new_env("threeparttable", latex2("\n", contents, "\n"))
        table <- set_contents(table, threepart)
        parsed[[table_info$tablePath]] <- table
      } else {
        table <- parsed[[table_info$tabularPath]]
        parsed[[table_info$tabularPath]] <-
          new_env("threeparttable", "\n", table, "\n",
                  new_env("tablenotes",
                          "\n\\small\n",
                          latex2(footer)), "\n")
      }
      table_info$tabularPath <- getTabularPath(parsed)
    } else {
      contents <- strsplit(deparseLatex(table), "\n")[[1]]
      table.width <- max(nchar(
          str_replace_all(contents, "\\[note[0-9]{0,2}\\]",
                          "")) + 2 * (table_info$ncol - 1))
      footer <- ""
      for (i in 1:count.label) {
        label.wrap <- strwrap(label[i], table.width)
        footer <- latex2(footer, "\\multicolumn",
                         new_block(table_info$ncol),
                         "{l}",
                         new_block(latex2("\\textsuperscript",
                                          new_block(ids[i]), " ",
                                          label.wrap[1])), "\\\\\n")
        if (length(label.wrap) > 1) {
          for (j in 2:length(label.wrap)) {
            footer <- latex2(footer, "\\multicolumn",
                             new_block(table_info$ncol),
                             "{l}", new_block(label.wrap[j]), "\n")
          }
        }
      }
      table <- insert_values(table, length(table),
                             after = TRUE, footer)
      parsed[[table_info$tabularPath]] <- table
    }
  }
  parsed
}

