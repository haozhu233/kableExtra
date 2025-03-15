#' Collapse repeated rows to multirow cell
#'
#' @description Collapse same values in columns into multirow cells. This
#' feature does similar things with `group_rows`. However, unlike `group_rows`,
#' it analyzes existing columns, finds out rows that can be grouped together,
#' and make them multirow cells. Note that if you want to use `column_spec` to
#' specify column styles, you should use `column_spec` before `collapse_rows`.
#'
#' @param kable_input Output of `knitr::kable()` with `format` specified
#' @param columns A numeric value or vector indicating in which column(s) rows
#' need to be collapsed.
#' @param valign Select from "top", "middle" (default), "bottom". The reason why
#' "top" is not default is that the multirow package on CRAN win-builder is
#' not up to date.
#' Only used when `row_group_label_position` is `identity`.
#' @param latex_hline Option controlling the behavior of adding hlines to table.
#' Choose from `full`, `major`, `none`, `custom` and `linespace`.
#' @param custom_latex_hline Numeric column positions whose collapsed rows will
#' be separated by hlines.
#' @param row_group_label_position Option controlling positions of row group
#' labels. Choose from `identity`, `stack`, or `first` -- the latter behaves
#' like `identity` when `valign` is `top` but without using the multirow
#' package.
#' @param row_group_label_fonts A list of arguments that can be supplied to
#' group_rows function to format the row group label when
#' `row_group_label_position` is `stack`.
#' @param headers_to_remove Numeric column positions where headers should be
#' removed when they are stacked.
#' @param target If multiple columns are selected to do collapsing and a target
#' column is specified, this target column will be used to collapse other
#' columns based on the groups of this target column.
#' @param col_names T/F. A LaTeX specific option. If you set `col.names` be
#' `NULL` in your `kable` call, you need to set this option false to let
#' everything work properly.
#' @param longtable_clean_cut T/F with default T. Multirow cell sometimes are
#' displayed incorrectly around pagebreak. This option forces groups to cut
#' before the end of a page. If you have a group that is longer than 1 page,
#' you need to turn off this option.
#'
#' @examples
#' \dontrun{
#' dt <- data.frame(a = c(1, 1, 2, 2), b = c("a", "a", "a", "b"))
#' x <- knitr::kable(dt, "html")
#' collapse_rows(x)
#' }
#'
#' @export
collapse_rows <- function(kable_input, columns = NULL,
                          valign = c("middle", "top", "bottom"),
                          latex_hline = c("full", "major", "none", "custom",
                                          "linespace"),
                          row_group_label_position = c("identity", "stack", "first"),
                          custom_latex_hline = NULL,
                          row_group_label_fonts = NULL,
                          headers_to_remove = NULL,
                          target = NULL,
                          col_names = TRUE,
                          longtable_clean_cut = TRUE) {
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
  valign <- match.arg(valign)
  if (!is.null(target)) {
    if (length(target) > 1 && is.integer(target)) {
      stop("target can only be a length 1 integer")
    }
  }
  if (kable_format == "html") {
    return(collapse_rows_html(kable_input, columns, valign, target))
  }
  if (kable_format == "latex") {
    latex_hline <- match.arg(latex_hline)
    row_group_label_position <- match.arg(row_group_label_position,
                                          c("identity", "stack", "first"))
    parsed <- kable_to_parsed(kable_input)
    res <- collapse_rows_latex(parsed, columns, latex_hline, valign,
                               row_group_label_position, row_group_label_fonts, custom_latex_hline,
                               headers_to_remove, target, col_names, longtable_clean_cut)
    parsed_to_kable(res, kable_input)
  }
}

collapse_rows_html <- function(kable_input, columns, valign, target) {
  kable_attrs <- attributes(kable_input)
  important_nodes <- read_kable_as_xml(kable_input)
  body_node <- important_nodes$body
  kable_xml <- important_nodes$table
  kable_tbody <- xml_tpart(kable_xml, "tbody")
  if (is.null(kable_tbody))
    return(kable_input)
  kable_dt <- read_table_data_from_xml(kable_xml)
  if (is.null(columns)) {
    columns <- seq(1, ncol(kable_dt))
  }
  if (!is.null(target)) {
    if (!target %in% columns) {
      stop("target has to be within the range of columns")
    }
  }
  collapse_matrix <- collapse_row_matrix(kable_dt, columns, target = target)

  for (i in 1:nrow(collapse_matrix)) {
    matrix_row <- collapse_matrix[i, ]
    names(matrix_row) <- names(collapse_matrix)
    target_row <- xml_child(kable_tbody, i)
    row_node_rm_count <- 0
    for (j in 1:length(matrix_row)) {
      collapsing_col <- as.numeric(sub("x", "", names(matrix_row)[j])) -
        row_node_rm_count
      target_cell <- xml_child(target_row, collapsing_col)
      if (matrix_row[j] == 0) {
        xml_remove(target_cell)
        row_node_rm_count <- row_node_rm_count + 1
      } else if (matrix_row[j] != 1) {
        xml_attr(target_cell, "rowspan") <- matrix_row[j]
        xml_attr(target_cell, "style") <- paste0(
          xml_attr(target_cell, "style"),
          "vertical-align: ", valign, " !important;")
      }
    }
  }

  out <- as_kable_xml(body_node)
  kable_attrs$collapse_matrix <- collapse_matrix
  attributes(out) <- kable_attrs
  if (!"kableExtra" %in% class(out)) class(out) <- c("kableExtra", class(out))
  return(out)
}

split_factor <- function(x) {
  group_idx <- seq(1, length(x))
  return(factor(unlist(lapply(group_idx, function(i) {rep(i, x[i])}))))
}

collapse_row_matrix <- function(kable_dt, columns, html = T, target = NULL)  {
  if (html) {
    column_block <- function(x) c(x, rep(0, x - 1))
  } else {
    column_block <- function(x) c(rep(0, x - 1), x)
  }
  mapping_matrix <- list()
  if (is.null(target)) {
    for (i in columns) {
      mapping_matrix[[paste0("x", i)]] <- unlist(lapply(
        rle(kable_dt[, i])$lengths, column_block))
    }
  } else {
    target_group = split_factor(rle(kable_dt[, target])$lengths)
    for (i in columns) {
      column_split = split(kable_dt[, i], target_group)
      mapping_matrix[[paste0("x", i)]] <- unlist(lapply(
        column_split, function(sp) {
          lapply(rle(sp)$length, column_block)
        }))
    }
  }

  mapping_matrix <- data.frame(mapping_matrix)
  return(mapping_matrix)
}

collapse_rows_latex <- function(parsed, columns, latex_hline, valign,
                                row_group_label_position, row_group_label_fonts,
                                custom_latex_hline, headers_to_remove, target,
                                col_names, longtable_clean_cut) {
  table_info <- magic_mirror(parsed)
  if (table_info$nrow <= 2) return(kable_input)

  table <- parsed[[table_info$tabularPath]]
  table <- remove_addlinespace(table)

  valign <- switch(
    valign,
    top = "[t]",
    middle = "",
    bottom = "[b]"
  )

  if (is.null(columns)) {
    columns <- seq(1, table_info$ncol)
  }

  kable_dt <- kable_dt_latex(table, col_names)

  collapse_matrix_rev <- collapse_row_matrix(kable_dt, columns, html = TRUE,
                                             target)
  collapse_matrix <- collapse_row_matrix(kable_dt, columns, html = FALSE,
                                         target)

  new_kable_dt <- kable_dt
  for (j in seq_along(columns)) {
    column_align <- table_info$align_vector_origin[columns[j]]
    column_width <- ifelse(
      is.null(table_info$column_width[[paste0("column_", columns[j])]]),
      "*", table_info$column_width[[paste0("column_", columns[j])]])
    for (i in seq(1:nrow(collapse_matrix))) {
      if(row_group_label_position == 'stack'){
        if(columns[j] < ncol(collapse_matrix) || collapse_matrix_rev[i, j] == 0){
          new_kable_dt[i, columns[j]] <- ""
        }
      } else if(row_group_label_position == 'first'){
        if(columns[j] <= ncol(collapse_matrix) && collapse_matrix_rev[i, j] == 0){
          new_kable_dt[i, columns[j]] <- ""
        }
      } else {
        # We need to account for the cmidrules that
        # are to the right of this column
        num_rows <- collapse_matrix[i, j]
        num_cols <- ncol(collapse_matrix) - j
        if (all(num_rows > 0, num_cols > 0, valign != "[b]", latex_hline != "none")) {
          vmove <- sum(rowSums(collapse_matrix[i - seq_len(num_rows-1), j + seq_len(num_cols), drop = FALSE]) > 0)
          if(valign == "") {
            vmove <- 0.5 * vmove
          }
        } else {
          vmove <- 0
        }
        new_kable_dt[i, columns[j]] <- collapse_new_dt_item(
          x = kable_dt[i, columns[j]], span = collapse_matrix[i, j], width = column_width,
          align = column_align, valign = valign,
          vmove = vmove, latex_hline = latex_hline
        )
      }
    }
  }

  midrule_matrix <- collapse_row_matrix(kable_dt, seq(1, table_info$ncol),
                                        html = FALSE, target)
  midrule_matrix[setdiff(seq(1, table_info$ncol), columns)] <- 1

  ex_bottom <- table_info$nrow - 1

  # # Remove rules
  # rules <- find_rules(table)
  # rules <- unlist(rules[-c(1:2, length(rules))])
  # table <- drop_items(table, rules)

  new_contents <- list()
  if(row_group_label_position == 'stack'){
    if(is.null(headers_to_remove)) headers_to_remove <- head(columns, -1)
    table_info$colnames[headers_to_remove] <- ""
    new_header <- paste(table_info$colnames, collapse = ' & ')
    out <- sub(contents[1], new_header, out)
    table_info$contents[1] <- new_header
  }
  if(latex_hline == 'custom' & is.null(custom_latex_hline)){
    if(row_group_label_position == 'stack'){
      custom_latex_hline = 1:2
    } else {
      custom_latex_hline = 1
    }
  }

  if (table_info$tabular == "longtable" && longtable_clean_cut) {
    if (max(collapse_matrix) > 50) {
      warning("It seems that you have a group larger than 50 rows and span ",
              "over a page. You probably want to set longtable_clean_cut to ",
              "be FALSE.")
    }
    pagebreak_hint <- "\\pagebreak[0]"
    nopagebreak <- "\\nopagebreak"
  } else {
    pagebreak_hint <- ""
    nopagebreak <- ""
  }

  rules <- find_rules(table)
  # Do the loop in reverse so that rules doesn't need
  # to be recalculated.
  for (i in rev(seq_len(nrow(collapse_matrix)))) {
    new_contents[[i]] <- vector_to_row(new_kable_dt[i, ])
    table_info$contents[[i + 1]] <- new_contents[[i]]
    if (i == 1) {
      row_midrule <- rule(table, 2)
    } else {
      row_midrule <- switch(
        latex_hline,
        "none" = "",
        "full" = paste0(
          midline_groups(which(as.numeric(midrule_matrix[i-1, ]) > 0),
                         table_info$booktabs),
          ifelse(
            sum(as.numeric(midrule_matrix[i-1, ]) > 0) == ncol(midrule_matrix),
            pagebreak_hint, nopagebreak
          )
        ),
        "major" = ifelse(
          sum(as.numeric(midrule_matrix[i-1, ]) > 0) == ncol(midrule_matrix),
              paste0(midline_groups(which(as.numeric(midrule_matrix[i-1, ]) > 0),
                                           table_info$booktabs), pagebreak_hint),
          nopagebreak
        ),
        "custom" = ifelse(
          sum(as.numeric(midrule_matrix[i-1, custom_latex_hline])) > 0,
          midline_groups(which(as.numeric(midrule_matrix[i-1, ]) > 0),
                         table_info$booktabs),
          ""
        ),
        "linespace"= ifelse(
          sum(as.numeric(midrule_matrix[i-1, ]) > 0) == ncol(midrule_matrix),
          "\\addlinespace",
          ""
        )
      )
    }
    if (row_midrule != "")
      new_contents[[i]] <- latex2(row_midrule, "\n", new_contents[[i]])
    table_info$contents[[i + 1]] <- new_contents[[i]]
    table <- drop_items(table, rules[[i+1]])
    tableRow(table, i + 1, withExtras = TRUE) <- latex2(tableRow(table, i + 1, withExtras = TRUE, withData = FALSE), new_contents[[i]])
  }
  parsed[[table_info$tabularPath]] <- table
  table_info$collapse_rows <- TRUE
  table_info$collapse_matrix <- collapse_matrix
  parsed <- update_meta(parsed, table_info)
  if(row_group_label_position == 'stack'){
    group_row_index_list <- collapse_rows_index(kable_dt, head(columns, -1))
    parsed <- collapse_rows_latex_stack(parsed, group_row_index_list, row_group_label_fonts)
  }
  parsed
}

kable_dt_latex <- function(table, col_names) {
  result <- matrix("", tableNrow(table), tableNcol(table))
  for (i in 1:nrow(result)) {
    result[i,] <- row_to_vector(tableRow(table, i))
  }
  if (col_names)
    result <- result[-1, ]
  result
}

collapse_new_dt_item <- function(x, span, width = NULL, align, valign,
                                 vmove = 0, latex_hline) {
  if (span == 0) return("")
  if (span == 1) return(x)
  out <- latex2(
    "\\multirow", valign,
    new_block(if (any(valign != "[t]",
                      !latex_hline %in% c("none", "major", "linespace"))) -span else (-(span - 1))),
    new_block(if (is.null(width)) "*" else width),
    switch(
      latex_hline,
      "full" = paste0(
        "[", span - 1,
        "\\dimexpr\\aboverulesep+\\belowrulesep+\\cmidrulewidth]"
      ),
      "custom" = paste0(
        "[", vmove,
        "\\dimexpr\\aboverulesep+\\belowrulesep+\\cmidrulewidth]"
      ),
      paste0("[\\normalbaselineskip]")
    ),
    new_block(latex2(
    switch(align,
           "l" = "\\raggedright\\arraybackslash",
           "c" = "\\centering\\arraybackslash",
           "r" = "\\raggedleft\\arraybackslash"),
    "  ",
    x))
  )
  deparseLatex(out)
}

midline_groups <- function(x, booktabs = TRUE) {
  diffs <- c(1, diff(x))
  start_indexes <- c(1, which(diffs > 1))
  end_indexes <- c(start_indexes - 1, length(x))
  ranges <- paste0(x[start_indexes], "-", x[end_indexes])
  if (booktabs) {
    out <- paste0("\\cmidrule{", ranges, "}")
  } else {
    out <- paste0("\\cline{", ranges, "}")
  }
  out <- paste0(out, collapse = "\n")
  return(out)
}

linespace_groups <- function(x) {
  diffs <- c(1, diff(x))
  start_indexes <- c(1, which(diffs > 1))
  end_indexes <- c(start_indexes - 1, length(x))
  ranges <- paste0(x[start_indexes], "-", x[end_indexes])
  out <- paste0("\\\\addlinespace")
  out <- paste0(out, collapse = "\n")
  return(out)
}


collapse_rows_index <- function(kable_dt, columns)  {
  format_to_row_index <- function(x){
    x = rle(x)
    out = x$lengths
    names(out) = x$values
    out
  }
  group_rows_index_list <- lapply(columns, function(x) {
    format_to_row_index(kable_dt[, x])
  })
  return(group_rows_index_list)
}


collapse_rows_latex_stack <- function(parsed, group_row_index_list,
                                      row_group_label_fonts){
  merge_lists <- function(default_list, updated_list){
    for(x in names(updated_list)){
      default_list[[x]] <- updated_list[[x]]
    }
    return(default_list)
  }
  default_font_list <- list(
    list(bold = T, italic = F),
    list(bold = F, italic = T),
    list(bold = F, italic = F)
  )
  n_default_fonts = length(default_font_list)
  n_supplied_fonts = length(row_group_label_fonts)
  group_row_font_list <- list()
  for(i in 1:length(group_row_index_list)){
    if(i > n_default_fonts){
      group_row_args <- default_font_list[[n_default_fonts]]
    } else {
      group_row_args <- default_font_list[[i]]
    }
    if(i <= n_supplied_fonts){
      group_row_args <- merge_lists(group_row_args, row_group_label_fonts[[i]])
    }
    group_row_args <- merge_lists(
      list(parsed = parsed, index = group_row_index_list[[i]], escape = FALSE),
      group_row_args)
    parsed <- do.call(group_rows2, group_row_args)
  }
  return(out)
}
