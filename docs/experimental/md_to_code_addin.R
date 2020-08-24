#' Convert selected RStudio markdown table to kable code
#'
#' @description RStudio 1.4 comes with a very nice live markdown table editor
#' (see https://bookdown.org/yihui/rmarkdown-cookbook/rstudio-visual.html for
#' details). For those who need to further customize those markdown tables, you
#' can use this function/addin to convert the markdown table to necessary R
#' code to render that table using `kableExtra`.
#'
#' @export
md_table_to_kable <- function() {
  current_selection <- rstudioapi::getSourceEditorContext()
  x <- current_selection$selection[[1]]$text
  if (x == "") {
    stop("You have not yet selected any markdown table text!")
  }
  x <- trimws(x)
  x_lines <- stringr::str_split(x, '\n')[[1]]
  if (!stringr::str_detect(x_lines[2], '^\\|[\\-\\:]')) {
    stop("Unexpected form of markdown table. This function only works for ",
         "regular markdown tables. Compact mode is not supported.")
  }

  # Get caption if available
  if (stringr::str_detect(x_lines[length(x_lines)], '^: ')) {
    caption <- sub('^: ', '', x_lines[length(x_lines)])
    x_lines <- x_lines[-c(length(x_lines) - 1, length(x_lines))]
  } else {
    caption <- NULL
  }
  n_row <- length(x_lines) - 2

  # get content matrix
  col_length <- stringr::str_split(x_lines[2], "\\|")[[1]]
  col_length <- col_length[-c(1, length(col_length))]
  n_col <- length(col_length)
  col_length <- nchar(col_length)
  col_offset <- cumsum(col_length + 1) + 1
  col_offset <- c(1, col_offset[-n_col])
  start_pos <- col_offset + 2
  end_pos <- col_length + col_offset - 1
  content_matrix <- matrix(
    unlist(lapply(x_lines, substring, start_pos, end_pos)),
    ncol = n_col, byrow = TRUE
    )

  # Get alignment
  alignment <- vapply(content_matrix[2, ], function(x) {
    if (stringr::str_sub(x, -1, -1) == ":") {
      if (stringr::str_sub(x, 1, 1) == ":") {
        return('c')
      } else {
        return('r')
      }
    } else {
      return('l')
    }
  }, 'character', USE.NAMES = FALSE)

  content_matrix <- trimws(content_matrix)
  col_names <- content_matrix[1, ]

  content_matrix <- content_matrix[-c(1, 2), ]
  content_matrix <- sim_double_escape(content_matrix)

  str_parts <- c(
    '```{r}\n',
    'data.frame(\n'
  )

  if (n_col > 1) {
    for (i in seq(n_col - 1)) {
      str_parts[i + 2] <- paste0(
        '  `Col', i, '` = ', vector_str(content_matrix[, i]), ', \n'
      )
    }
  }
  str_parts[n_col + 2] <- paste0(
    '  `Col', n_col, '` = ', vector_str(content_matrix[, n_col]), '\n'
  )
  str_parts[n_col + 3] <- paste0(
    ') %>%\n  kableExtra::kbl(\n    escape = F, \n    col.names = ',
    if (all(col_names == "")) 'NULL' else vector_str(col_names),
    ', \n    align = ', vector_str(alignment), ', \n    caption = ',
    if (is.null(caption)) 'NULL' else paste0('"', caption, '"'),
    ',\n    booktable = T\n  ) %>%\n  kableExtra::kable_styling()\n```'
  )
  out <- paste(str_parts, collapse = "")

  rstudioapi::insertText(location = current_selection$selection[[1]]$range,
                         text = out,
                         id = current_selection$id)

  return(out)
}

vector_str <- function(v) {
  return(paste0('c(', paste0('"', v, '"', collapse = ", "), ')'))
}
