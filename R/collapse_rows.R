#' Collapse repeat rows to multirow cell
#'
#' @export
collapse_rows <- function(kable_input, columns) {
  if (is.null(columns)) {
    stop("Please specify numeric positions of columns you want to collapse.")
  }
  kable_format <- attr(kable_input, "format")
  if (!kable_format %in% c("html", "latex")) {
    message("Currently generic markdown table using pandoc is not supported.")
    return(kable_input)
  }
  if (kable_format == "html") {
    return(collapse_rows_html(kable_input, columns))
  }
  if (kable_format == "latex") {
    return(collapse_rows_latex(kable_input, columns))
  }
}

collapse_rows_html <- function(kable_input, columns) {
  kable_attrs <- attributes(kable_input)
  kable_xml <- read_xml(as.character(kable_input), options = "COMPACT")
  kable_tbody <- xml_tpart(kable_xml, "tbody")

  kable_dt <- rvest::html_table(xml2::read_html(as.character(kable_input)))[[1]]
  kable_dt$row_id <- rownames(kable_dt)
  collapse_matrix <- collapse_row_matrix(kable_dt, columns)

  for (i in 1:nrow(collapse_matrix)) {
    matrix_row <- collapse_matrix[i, ]
    if (sum(matrix_row) != length(matrix_row)) {
      target_row <- xml_child(kable_tbody, i)
      row_node_rm_count <- 0
      for (j in 1:length(matrix_row)) {
        if (matrix_row[j] != 1) {
          collapsing_col <- as.numeric(sub("x", "", names(matrix_row)[j])) -
            row_node_rm_count
          target_cell <- xml_child(target_row, collapsing_col)
          if (matrix_row[j] == 0) {
            xml_remove(target_cell)
            row_node_rm_count <- row_node_rm_count + 1
          } else {
            xml_attr(target_cell, "rowspan") <- matrix_row[j]
            xml_attr(target_cell, "style") <- paste0(
              xml_attr(target_cell, "style"),
              "vertical-align: middle !important;")
          }
        }
      }
    }
  }

  out <- structure(as.character(kable_xml), format = "html",
                   class = "knitr_kable")
  attributes(out) <- kable_attrs
  return(out)
}

collapse_rows_latex <- function(kable_input, columns) {
  # table_info <- magic_mirror(kable_input)
  # target_row <- table_info$contents[row + 1]
  # new_row <- latex_row_cells(target_row)
  # if (bold) {
  #   new_row <- lapply(new_row, function(x) {
  #     paste0("\\\\bfseries{", x, "}")
  #   })
  # }
  # if (italic) {
  #   new_row <- lapply(new_row, function(x) {
  #     paste0("\\\\em{", x, "}")
  #   })
  # }
  # new_row <- paste(unlist(new_row), collapse = " & ")
  #
  # out <- sub(target_row, new_row, as.character(kable_input), perl = T)
  # out <- structure(out, format = "latex", class = "knitr_kable")
  # attr(out, "original_kable_meta") <- table_info
  # return(out)
  kable_input
}
