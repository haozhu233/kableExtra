#' Add a heading column to the left side of the table
#'
#' @description This function uses the same syntax as add_header_above. It will
#' add a heading column with grouped rows to the left side of the table. It can
#' act as an alternative way to `group_rows` to show grouped information. As
#' `add_header_above`, users can use this function to add multiple layers of
#' heading columns one by one.
#'
#' @param kable_input Output of `knitr::kable()` with `format` specified
#' @param header A (named) character vector with `rowspan` as values. For
#' example, `c("xxx" = 1, "title" = 2)` can be used to create a new header column
#' for a 3-row table with "xxx" spanning row 1 and "title" spanning row 2 & 3 (
#' two rows). For convenience, when `rowspan` equals to 1, users can drop the
#' ` = 1` part. As a result, `c("xxx", "title" = 2)` is the same as
#' `c("xxx" = 1, "title" = 2)`.
#' @param header_name Column name that that extra column
#' @param width A character string for the width of the new column. Values
#' could be "10cm", "3in" or "30em", etc..
#' @param align Column alignment. you can choose from "c", "l" or "r"
#' @param bold A T/F value to control whether the text should be bolded.
#' @param italic A T/F value to control whether the text should to be emphasized.
#' @param ... Extra options to be passed into HTML or LaTeX. Right now there is
#' only one for LaTeX. Option full_midline is a TRUE/FALSE option to control
#' if the mid line needs to be extended to the end of row.
#'
#' @examples x <- knitr::kable(head(mtcars), "html")
#' add_header_left(x, c("A" = 2, "B" = 2, "C" = 2))
add_header_left <- function(kable_input, header = NULL, header_name = "",
                            align = "c", width = NULL, bold = F, italic = F,
                            ...) {
  if (is.null(header)) return(kable_input)
  kable_format <- attr(kable_input, "format")
  if (!kable_format %in% c("html", "latex")) {
    stop("Please specify output format in your kable function. Currently ",
         "generic markdown table using pandoc is not supported.")
  }
  if (kable_format == "html") {
    return(add_header_left_html(kable_input, header, header_name, align,
                                width, bold, italic))
  }
  if (kable_format == "latex") {
    return(add_header_left_latex(kable_input, header, header_name, align,
                                 width, bold, italic, ...))
  }
}

# HTML
add_header_left_html <- function(kable_input, header, header_name, align,
                                 width, bold, italic) {
  kable_attrs <- attributes(kable_input)
  kable_xml <- read_xml(as.character(kable_input), options = "COMPACT")
  kable_thead <- xml_tpart(kable_xml, "thead")
  kable_tbody <- xml_tpart(kable_xml, "tbody")

  align <- match.arg(align, c("c", "l", "r"))
  align <- switch(align, "c" = "center", "l" = "left", "r" = "right")

  column_style <- paste0(
    ifelse(!is.null(width), paste0("width: ", width, "; "), ""),
    ifelse(bold, "font-weight: bold; ", ""),
    ifelse(italic, "font-style: italic; ", "")
  )

  new_header <- paste0(
    '<th style="text-align:', align, '; vertical-align: bottom;', column_style,
    '" rowspan="', length(xml_children(kable_thead)), '">', header_name, '</th>'
  )
  new_header <- read_xml(new_header, options = c("COMPACT"))
  xml_add_child(xml_child(kable_thead, 1), new_header, .where = 0)

  header <- standardize_header(header, length(xml_children(kable_tbody)))
  for (i in 1:nrow(header)) {
    new_row_item <- paste0(
      '<td style="text-align:', align, '; vertical-align: middle;',
      column_style, '" rowspan="',
      header$rowspan[i], '">', header$header[i], '</td>')
    new_row_item <- read_xml(new_row_item, options = "COMPACT")
    target_row <- xml_child(kable_tbody, header$row[i])
    xml_add_child(target_row, new_row_item, .where = 0)
  }

  out <- structure(as.character(kable_xml), format = "html",
                   class = "knitr_kable")

  # Adjust for column_spec
  if (is.null(kable_attrs$column_adjust)) {
    table_nrow <- length(xml_children(kable_tbody))
    # if (!is.null(kable_attrs$group_header_rows)) {
    #   table_nrow <- table_nrow - length(kable_attrs$group_header_rows)
    # }
    table_ncol <- length(xml_children(
      xml_child(kable_thead, length(xml_children(kable_thead)))
    ))
    kable_attrs$column_adjust$matrix <- matrix(
      rep(TRUE, table_nrow * table_ncol), ncol = table_nrow)
    kable_attrs$column_adjust$count <- 1
    new_row_index <- rep(FALSE, table_nrow)
  } else {
    new_row_index <- rep(FALSE, ncol(kable_attrs$column_adjust$matrix))
    kable_attrs$column_adjust$count <- 1 + kable_attrs$column_adjust$count
  }
  new_row_index[header$row] <- TRUE
  kable_attrs$column_adjust$matrix <- rbind(
    new_row_index, kable_attrs$column_adjust$matrix
  )
  attributes(out) <- kable_attrs

  return(out)
}

standardize_header <- function(header, n_row) {
  header_names <- names(header)

  if (is.null(header_names)) {
    return(data.frame(header = header, row = 1:length(header),
                      rowspan = 1, row.names = NULL))
  }

  names(header)[header_names == ""] <- header[header_names == ""]
  header[header_names == ""] <- 1
  header_names <- names(header)
  header <- as.numeric(header)
  names(header) <- header_names
  if (sum(header) < n_row) {
    header <- c(header, " " = n_row - sum(header))
  }
  row_pos <- c(1, cumsum(header)[-length(header)] + 1)
  return(data.frame(
    header = names(header),
    row = row_pos, rowspan = header, row.names = NULL
    ))
}

add_header_left_latex <- function(kable_input, header, header_name, align,
                                  width, bold, italic, full_midline = F) {
  table_info <- magic_mirror(kable_input)
  usepackage_latex("multirow")
  if (!table_info$booktabs) {
    warning("add_header_left only supports LaTeX table with booktabs. Please",
            " use kable(..., booktabs = T) in your kable function.")
  }
  out <- as.character(kable_input)
  contents <- table_info$contents
  header_name <- escape_latex(header_name)
  header <- standardize_header(header, length(contents) - 1)
  header$header <- escape_latex(header$header)
  header$header <- gsub("\\\\", "\\\\\\\\", header$header)
  header$row <- header$row + 1
  header$row_end <- header$row + header$rowspan - 1

  # Align
  align_row <- latex_column_align_builder(align, width, bold, italic)

  out <- sub(paste0(table_info$begin_tabular, "\\{"),
             paste0(table_info$begin_tabular, "{", align_row,
                    ifelse(table_info$booktabs, "", "|")),
             out, perl = T)
  # table_info$align_vector <- c(align, table_info$align_vector)

  # Header
  ## Extra header rows introduced by add_header_above
  if (!is.null(table_info$new_header_row)) {
    new_header_row <- table_info$new_header_row
    for (i in 1:length(new_header_row)) {
      out <- sub(regex_escape(new_header_row[i]),
                 paste0(" & ", new_header_row[i]), out)
      cline_old <- cline_gen(table_info$header_df[[i]], table_info$booktabs)
      cline_old <- regex_escape(cline_old)
      table_info$header_df[[i]] <- rbind(
        data.frame(header = " ", colspan = 1),
        table_info$header_df[[i]]
      )
      cline_new <- cline_gen(table_info$header_df[[i]], table_info$booktabs)
      out <- sub(cline_old, cline_new, out)
    }
  }
  ## Base Header row
  out <- sub(contents[1], paste0(header_name, " & ", contents[1]), out)
  table_info$contents[1] <- paste0(header_name, " & ", contents[1])

  # move existing midrules if exists
  out_lines <- read_lines(out)
  tbody_start_row <- which(out_lines == "\\midrule")
  tbody_end_row <- which(out_lines == "\\bottomrule")
  before_tbody <- out_lines[seq(1, tbody_start_row)]
  tbody <- out_lines[seq(tbody_start_row + 1, tbody_end_row - 1)]
  after_tbody <- out_lines[seq(tbody_end_row, length(out_lines))]

  # Remove addlinespace in this case
  tbody <- tbody[tbody != "\\addlinespace"]

  midrule_exist <- str_sub(tbody, 1, 9) == "\\cmidrule"
  if (sum(midrule_exist) > 0) {
    existing_midrules <- which(midrule_exist)
    tbody[existing_midrules] <- unlist(lapply(
      tbody[existing_midrules], cmidrule_plus_one
    ))
    out <- paste0(c(before_tbody, tbody, after_tbody), collapse = "\n")
  }

  for (j in 1:nrow(header)) {
    new_row_pre <- paste0(
      "\\\\multirow\\{", -header$rowspan[j], "\\}\\{",
      ifelse(is.null(width), "\\*", width),
      "\\}\\{",
      switch(align,
             "l" = "\\\\raggedright\\\\arraybackslash ",
             "c" = "\\\\centering\\\\arraybackslash ",
             "r" = "\\\\raggedleft\\\\arraybackslash "),
      header$header[j], "\\} & "
    )
    new_row_text <- paste0(new_row_pre, contents[header$row_end[j]])
    out <- sub(contents[header$row_end[j]], new_row_text, out)
    table_info$contents[header$row_end[j]] <- new_row_text
    if (j != nrow(header)) {
      out <- sub(
        paste0(contents[header$row_end[j]], "\\\\\\\\\n"),
        paste0(contents[header$row_end[j]],
               "\\\\\\\\\n\\\\cmidrule[0.5pt](l{2pt}r{2pt}){1-",
               ifelse(full_midline, str_count(tbody[1], " & ") + 2, 1),
               "}\n"),
        out
      )
    }
  }

  for (k in setdiff(seq(2, length(contents)), header$row_end)) {
    out <- sub(contents[k],
               paste0("  & ", contents[k]),
               out)
    table_info$contents[k] <- paste0("  & ", contents[k])
  }

  out <- structure(out, format = "latex", class = "knitr_kable")

  attr(out, "kable_meta") <- table_info
  return(out)
}

cmidrule_plus_one <- function(x) {
  start_pos <- as.numeric(str_match(x, "\\)\\{(.*)-")[2]) + 1
  stop_pos <- as.numeric(str_match(x, "-(.*)\\}")[2]) + 1
  return(
    paste0("\\cmidrule[0.5pt](l{2pt}r{2pt}){", start_pos, "-", stop_pos, "}")
  )
}
