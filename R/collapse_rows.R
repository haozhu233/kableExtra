#' Collapse repeated rows to multirow cell
#'
#' @description Collapse same values in columns into multirow cells. This
#' feature does similar things with `group_rows`. However, unlike `group_rows`,
#' it analyzes existing columns, finds out rows that can be grouped together,
#' and make them multirow cells. Note that if you want to use `column_spec` to
#' specify column styles, you should use `column_spec` before `collapse_rows`.
#'
#' @param kable_input Output of `knitr::kable()` with `format` specified
#' @param columns Numeric column positions where rows need to be collapsed.
#' @param latex_hline Option controlling the behavior of adding hlines to table.
#' Choose from `full`, `major`, `none`, `custom`.
#' @param custom_latex_hline Numeric column positions whose collapsed rows will
#' be separated by hlines.
#' @param row_group_label_position Option controlling positions of row group
#' labels. Choose from `identity`, `stack`.
#' @param row_group_label_fonts A list of arguments that can be supplied to
#' group_rows function to format the row group label when
#' `row_group_label_position` is `stack`
#' @param headers_to_remove Numeric column positions where headers should be
#' removed when they are stacked.
#'
#' @examples dt <- data.frame(a = c(1, 1, 2, 2), b = c("a", "a", "a", "b"))
#' x <- knitr::kable(dt, "html")
#' collapse_rows(x)
#'
#' @export
collapse_rows <- function(kable_input, columns = NULL,
                          latex_hline = c("full", "major", "none", "custom"),
                          row_group_label_position = c('identity', 'stack'),
                          custom_latex_hline = NULL,
                          row_group_label_fonts = NULL,
                          headers_to_remove = NULL) {
  # if (is.null(columns)) {
  #   stop("Please specify numeric positions of columns you want to collapse.")
  # }
  kable_format <- attr(kable_input, "format")
  if (!kable_format %in% c("html", "latex")) {
    warning("Please specify format in kable. kableExtra can customize either ",
            "HTML or LaTeX outputs. See https://haozhu233.github.io/kableExtra/ ",
            "for details.")
    return(kable_input)
  }
  if (kable_format == "html") {
    return(collapse_rows_html(kable_input, columns))
  }
  if (kable_format == "latex") {
    latex_hline <- match.arg(latex_hline, c("full", "major", "none", "custom"))
    row_group_label_position <- match.arg(row_group_label_position,
                                          c('identity', 'stack'))
    return(collapse_rows_latex(kable_input, columns, latex_hline,
      row_group_label_position, row_group_label_fonts, custom_latex_hline,
      headers_to_remove))
  }
}

collapse_rows_html <- function(kable_input, columns) {
  kable_attrs <- attributes(kable_input)
  kable_xml <- read_kable_as_xml(kable_input)
  kable_tbody <- xml_tpart(kable_xml, "tbody")

  kable_dt <- rvest::html_table(xml2::read_html(as.character(kable_input)))[[1]]
  if (is.null(columns)) {
    columns <- seq(1, ncol(kable_dt))
  }
  if (!is.null(kable_attrs$header_above)) {
    kable_dt_col_names <- unlist(kable_dt[kable_attrs$header_above, ])
    kable_dt <- kable_dt[-(1:kable_attrs$header_above),]
    names(kable_dt) <- kable_dt_col_names
  }
  kable_dt$row_id <- seq(nrow(kable_dt))
  collapse_matrix <- collapse_row_matrix(kable_dt, columns)

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
          "vertical-align: middle !important;")
      }
    }
  }

  out <- as_kable_xml(kable_xml)
  attributes(out) <- kable_attrs
  if (!"kableExtra" %in% class(out)) class(out) <- c("kableExtra", class(out))
  return(out)
}

collapse_row_matrix <- function(kable_dt, columns, html = T)  {
  if (html) {
    column_block <- function(x) c(x, rep(0, x - 1))
  } else {
    column_block <- function(x) c(rep(0, x - 1), x)
  }
  mapping_matrix <- list()
  for (i in columns) {
    mapping_matrix[[paste0("x", i)]] <- unlist(lapply(
      rle(kable_dt[, i])$length, column_block))
  }
  mapping_matrix <- data.frame(mapping_matrix)
  return(mapping_matrix)
}

collapse_rows_latex <- function(kable_input, columns, latex_hline,
                                row_group_label_position, row_group_label_fonts,
                                custom_latex_hline, headers_to_remove) {
  table_info <- magic_mirror(kable_input)
  out <- enc2utf8(as.character(kable_input))

  if (is.null(columns)) {
    columns <- seq(1, table_info$ncol)
  }

  contents <- table_info$contents
  kable_dt <- kable_dt_latex(contents)

  collapse_matrix_rev <- collapse_row_matrix(kable_dt, columns, html = TRUE)
  collapse_matrix <- collapse_row_matrix(kable_dt, columns, html = FALSE)

  new_kable_dt <- kable_dt
  for (j in seq(1:ncol(collapse_matrix))) {
    column_align <- table_info$align_vector_origin[columns[j]]
    column_width <- ifelse(
      is.null(table_info$column_width[[paste0("column_", columns[j])]]),
      "*", table_info$column_width[paste0("column_", columns[j])])
    for (i in seq(1:nrow(collapse_matrix))) {
      if(row_group_label_position == 'stack'){
        if(j < ncol(collapse_matrix)|(collapse_matrix_rev[i, j] == 0)){
          new_kable_dt[i, j] <- ''
        }
      } else {
        new_kable_dt[i, j] <- collapse_new_dt_item(
          kable_dt[i, j], collapse_matrix[i, j], column_width, align = column_align
        )
      }
    }
  }

  midrule_matrix <- collapse_row_matrix(kable_dt, seq(1, table_info$ncol),
                                        html = F)
  midrule_matrix[setdiff(seq(1, table_info$ncol), columns)] <- 1

  ex_bottom <- length(contents) - 1
  contents[2:ex_bottom] <- paste0(contents[2:ex_bottom], "\\\\\\\\")
  if (!table_info$booktabs) {
    contents[2:ex_bottom] <- paste0(contents[2:ex_bottom], "\n\\\\hline")
  }

  new_contents <- c()
  if(row_group_label_position == 'stack'){
    if(is.null(headers_to_remove)) headers_to_remove <- head(columns, -1)
    table_info$colnames[headers_to_remove] <- ''
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
  for (i in seq(1:nrow(collapse_matrix))) {
    new_contents[i] <- paste0(new_kable_dt[i, ], collapse = " & ")
    table_info$contents[i + 1] <- new_contents[i]
    if (i != nrow(collapse_matrix)) {
      row_midrule <- switch(
        latex_hline,
        "none" = "",
        "full" = midline_groups(which(as.numeric(midrule_matrix[i, ]) > 0),
                                table_info$booktabs),
        "major" = ifelse(
          sum(as.numeric(midrule_matrix[i, ]) > 0) == ncol(midrule_matrix),
          midline_groups(which(as.numeric(midrule_matrix[i, ]) > 0),
                         table_info$booktabs),
          ""
        ),
         "custom" = ifelse(
          sum(as.numeric(midrule_matrix[i, custom_latex_hline])) > 0,
          midline_groups(which(as.numeric(midrule_matrix[i, ]) > 0),
                         table_info$booktabs),
          ""
         )
      )
      new_contents[i] <- paste0(new_contents[i], "\\\\\\\\\n", row_midrule)
    }
    out <- sub(contents[i + 1], new_contents[i], out)
  }
  out <- gsub("\\\\addlinespace\n", "", out)

  out <- structure(out, format = "latex", class = "knitr_kable")
  table_info$collapse_rows <- TRUE
  attr(out, "kable_meta") <- table_info
  if(row_group_label_position == 'stack'){
    group_row_index_list <- collapse_rows_index(kable_dt, head(columns, -1))
    out <- collapse_rows_latex_stack(out, group_row_index_list, row_group_label_fonts)
  }
  return(out)
}

kable_dt_latex <- function(x) {
  data.frame(do.call(rbind, str_split(x[-1], " & ")), stringsAsFactors = FALSE)
}

collapse_new_dt_item <- function(x, span, width = NULL, align) {
  if (span == 0) return("")
  if (span == 1) return(x)
  out <- paste0(
    "\\\\multirow\\{", -span, "\\}\\{",
    ifelse(is.null(width), "\\*", width),
    "\\}\\{",
    switch(align,
           "l" = "\\\\raggedright\\\\arraybackslash ",
           "c" = "\\\\centering\\\\arraybackslash ",
           "r" = "\\\\raggedleft\\\\arraybackslash "),
    x, "\\}"
  )
  return(out)
}

midline_groups <- function(x, booktabs = T) {
  diffs <- c(1, diff(x))
  start_indexes <- c(1, which(diffs > 1))
  end_indexes <- c(start_indexes - 1, length(x))
  ranges <- paste0(x[start_indexes], "-", x[end_indexes])
  if (booktabs) {
    out <- paste0("\\\\cmidrule{", ranges, "}")
  } else {
    out <- paste0("\\\\cline{", ranges, "}")
  }
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


collapse_rows_latex_stack <- function(kable_input, group_row_index_list,
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
  out <- kable_input
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
      list(kable_input = out, index = group_row_index_list[[i]]),
      group_row_args)
    out <- do.call(group_rows, group_row_args)
  }
  return(out)
}
