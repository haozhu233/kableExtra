#' R Markdown Format
#'
#' @description Check if the export format of the R Markdown document exists.
#'
#' @importFrom rmarkdown metadata
#'
#' @export

rmd_format <- function(){
  rmd_output_metadata <- metadata$output
  return(names(rmd_output_metadata))
}

#' Load a LaTeX package
#'
#' @description Load a LaTeX package using R code. Just like `\\usepackage{}`
#' in LaTeX
#'
#' @param name The LaTeX package name
#' @param options The LaTeX options for the package
#'
#' @examples usepackage_latex("xcolor")
#' @export
usepackage_latex <- function(name, options = NULL) {
  invisible(knit_meta_add(list(latex_dependency(name, options))))
}

#' Declare LaTeX packages needed by kableExtra
#'
#' @description
#' Declares all of the LaTeX packages that
#' may be used by `kableExtra` functions so that they
#' will be loaded when the document is produced.
#' @details
#' When `kableExtra` loads, it calls this function if it
#' detects that `knitr` is running and producing
#' LaTeX output.  However, sometimes `kableExtra`
#' is loaded before `knitr` runs, and then these packages
#' can end up being missed, leading to LaTeX errors such as
#' "Undefined control sequence."  (See
#' GitHub issue #721 for an example.)
#'
#' Our `kbl()` wrapper for `knitr::kable()` calls
#' this function for LaTeX output, so an explicit call
#' is not necessary.
#'
#' @examples use_latex_packages()
#' @export
use_latex_packages <- function() {
  load_packages <- getOption("kableExtra.latex.load_packages", default = TRUE)
  if (load_packages) {
    usepackage_latex("booktabs")
    usepackage_latex("longtable")
    usepackage_latex("array")
    usepackage_latex("multirow")
    usepackage_latex("wrapfig")
    usepackage_latex("float")
    usepackage_latex("colortbl")
    usepackage_latex("pdflscape")
    usepackage_latex("tabu")
    usepackage_latex("threeparttable")
    usepackage_latex("threeparttablex")
    usepackage_latex("ulem", "normalem")
    usepackage_latex("makecell")
    usepackage_latex("xcolor")
  }
}

# Find the right xml section. Since xml_child + search name will result in a
# crash (at least on my machine), here is a helper function.
xml_tpart <- function(x, part) {
  xchildren <- xml_children(x)
  children_names <- xml_name(xchildren)
  if(!part %in% children_names) return(NULL)
  return(xchildren[[which(children_names == part)]])
}

positions_corrector <- function(positions, group_header_rows, n_row) {
  pc_matrix <- data.frame(row_id = 1:n_row)
  pc_matrix$group_header <- pc_matrix$row_id %in% group_header_rows
  pc_matrix$adj <- cumsum(pc_matrix$group_header)
  pc_matrix$old_id <- cumsum(!pc_matrix$group_header)
  pc_matrix$old_id[duplicated(pc_matrix$old_id)] <- NA
  adjust_numbers <- pc_matrix$adj[pc_matrix$old_id %in% positions]
  return(adjust_numbers + positions)
}

latex_row_cells <- function(x) {
  stringr::str_split(x, " \\& ")
}

regex_escape <- function(x, double_backslash = FALSE) {
  if (double_backslash) {
    x <- gsub("\\\\", "\\\\\\\\", x)
  }
  x <- gsub("\\$", "\\\\\\$", x)
  x <- gsub("\\(", "\\\\(", x)
  x <- gsub("\\)", "\\\\)", x)
  x <- gsub("\\[", "\\\\[", x)
  x <- gsub("\\]", "\\\\]", x)
  x <- gsub("\\{", "\\\\{", x)
  x <- gsub("\\}", "\\\\}", x)
  x <- gsub("\\*", "\\\\*", x)
  x <- gsub("\\+", "\\\\+", x)
  x <- gsub("\\?", "\\\\?", x)
  x <- gsub("\\|", "\\\\|", x)
  x <- gsub("\\^", "\\\\^", x)
  x <- gsub("\\.", "\\\\.", x)
  return(x)
}

regex_unescape <- function(x) {
  sapply(x, function(y) sub(".", y, "."))
}

as_kable_xml <- function(bodynode) {
  out <- structure(as.character(bodynode),
                   format = "html", class = "knitr_kable")
  return(out)
}

child_to_character <- function(x) {
  result <- character()
  n <- xml_length(x)
  for (i in seq_len(n)) {
    if (xml_name(xml_child(x, i)) != "meta")
      result <- c(result, as.character(xml_child(x, i)))
  }
  result
}

dfs <- function(node, node_name='table') {
  if (is.null(node)) return(NULL)
  if (xml_name(node) == node_name) return(node)
  for (child in xml_children(node)) {
    found <- dfs(child, node_name)
    if (!is.null(found)) {
      return(found)
    }
  }
  return(NULL)
}

read_kable_as_xml <- function(x) {
  source_node <- read_html(as.character(x), options = c("RECOVER", "NOERROR"))
  body_node <- xml_children(dfs(source_node, 'body'))
  table_node <- dfs(source_node, 'table')
  if (is.null(table_node)) {
    stop('Did not find a HTML table tag in the provided HTML. ')
  }
  return(list(body=body_node, table=table_node))
}

get_xml_text <- function(xml_node) {
  return(trimws(xml2::xml_text(xml_node)))
}

read_table_data_from_xml <- function(kable_xml) {
  thead <- xml_tpart(kable_xml, "thead")
  tbody <- xml_tpart(kable_xml, "tbody")

  if (is.null(tbody))
    return(NULL)

  # Header part
  if (!is.null(thead)) {
    n_header_rows <- xml2::xml_length(thead)
    col_headers_xml <- xml2::xml_children(xml2::xml_child(thead, n_header_rows))
    col_headers <- unlist(lapply(col_headers_xml, get_xml_text))
    n_cols <- length(col_headers)
    first_column_as_row_names <- (col_headers[1] == '')
  } else {
    first_column_as_row_names <- FALSE
    col_headers <- NULL
    # We have no header, so get the maximum number of columns in the body
    n_cols <- vapply(xml2::xml_children(tbody),
                     function(row) length(xml2::xml_children(row)),
                     1L)
    n_cols <- max(n_cols)
  }

  # Content part
  filtered_rows <- lapply(xml2::xml_children(tbody), function(row) {
    all_tds <- xml2::xml_children(row)
    if (length(all_tds) == n_cols) {
      all_td_texts <- unlist(lapply(all_tds, get_xml_text))
      return(all_td_texts)
    } else {
      return(NULL)
    }
  })
  filtered_rows[sapply(filtered_rows, is.null)] <- NULL

  table_data <- do.call(rbind, filtered_rows)

  if (first_column_as_row_names) {
    row_names <- table_data[, 1]
    table_data <- table_data[, 2:ncol(table_data)]
    row.names(table_data) <- row_names
    colnames(table_data) <- col_headers[2:n_cols]
  } else {
    colnames(table_data) <- col_headers
  }
  return(as.data.frame(table_data))
}

#' LaTeX Packages
#' @param xelatex Is `xelatex` going to be used to process
#' the file?
#' @description This function shows all LaTeX packages that is supposed to be
#' loaded for this package in a R Markdown YAML format.
#'
#' @export
kableExtra_latex_packages <- function(xelatex = FALSE) {

  pkg_list <- paste0("  - ", latex_pkg_list(xelatex))

  pkg_text <- paste0(
    "header-includes:\n",
    paste0(pkg_list, collapse = "\n")
  )

  cat(pkg_text)
}

latex_pkg_list <- function(xelatex = FALSE) {
  return(c(
    "\\usepackage{booktabs}",
    "\\usepackage{longtable}",
    "\\usepackage{array}",
    "\\usepackage{multirow}",
    "\\usepackage{wrapfig}",
    "\\usepackage{float}",
    "\\usepackage{colortbl}",
    "\\usepackage{pdflscape}",
    "\\usepackage{tabu}",
    "\\usepackage{threeparttable}",
    "\\usepackage{threeparttablex}",
    "\\usepackage[normalem]{ulem}",
    if (!xelatex) "\\usepackage[utf8]{inputenc}",
    "\\usepackage{makecell}",
    "\\usepackage{xcolor}"
  ))
}

# Fix duplicated rows in LaTeX tables
fix_duplicated_rows_latex <- function(kable_input, table_info) {
  # Since sub/string_replace start from beginning, we count unique value from
  # behind.
  rev_contents <- rev(table_info$contents)
  dup_index <- rev(ave(seq_along(rev_contents), rev_contents,
                       FUN = seq_along))
  for (i in which(dup_index != 1)) {
    dup_row <- table_info$contents[i]
    empty_times <- dup_index[i] - 1
    # insert empty_times before last non whitespace characters
    new_row <- str_replace(
      dup_row, "(?<=\\s)([\\S]+[\\s]*)$",
      paste0("\\\\\\\\vphantom\\\\{", empty_times, "\\\\} \\1"))
    kable_input <- sub(
      paste0(dup_row, "(?=\\s*\\\\\\\\\\*?(\\[.*\\])?)"),
      new_row,
      kable_input,
      perl = TRUE)
    table_info$contents[i] <- new_row
  }
  table_info$duplicated_rows <- FALSE
  return(list(kable_input, table_info))
}

# Solve enc issue for LaTeX tables
solve_enc <- function(x) {
  if (Encoding(x) == "UTF-8"){
    out <- x
  } else {
  #may behave differently based on Sys.setlocale settings with respect to characters
    out <- enc2utf8(as.character(base::format(x, trim = TRUE, justify = 'none')))
  }
  mostattributes(out) <- attributes(x)
  return(out)
}

input_escape <- function(x, latex_align) {
  x <- escape_latex2(x)
  x <- linebreak(x, align = latex_align, double_escape = TRUE)
}

clear_color_latex <- function(x, background = F) {
  term <- if (background) "cellcolor" else "textcolor"
  regex_1 <- sprintf(
    "\\\\\\\\%s\\\\\\[HTML\\\\\\]\\\\\\{[a-zA-Z0-9]*\\\\\\}\\\\\\{", term
  )
  regex_2 <- sprintf(
    "\\\\\\\\%s\\\\\\{[a-zA-Z0-9]*\\\\\\}\\\\\\{", term
  )
  origin_len <- nchar(x)
  x <- stringr::str_remove(x, regex_1)
  x <- stringr::str_remove(x, regex_2)
  return(ifelse(nchar(x) != origin_len, stringr::str_remove(x, "\\\\\\}$"), x))
}

sim_double_escape <- function(x) {
  return(sub("\\\\", "\\\\\\\\", x))
}

sim_all_double_escape <- function(x) {
  return(gsub("\\\\", "\\\\\\\\", x))
}

# Here (v 1.4.0) we introduced a simple markdown table parser to compensate the
# breaking change on changing the default of auto_format.
line_separator <- function(line, idx_matrix) {
  return(trimws(apply(idx_matrix, 1, function(idx) {
    substr(line, idx[1], idx[2])
  })))
}

separator_indices <- function(line) {
  separator_indices <- which(strsplit(line, '')[[1]] == '|')
  cell_start_indices <- separator_indices[-length(separator_indices)] + 1
  cell_end_indices <- separator_indices[-1] - 1
  matrix(c(cell_start_indices, cell_end_indices), ncol=2)
}

md_table_parser <- function(md_table) {
  # It seems that if there is a caption, the second row is definitely an empty
  # string
  # https://github.com/yihui/knitr/blob/a51a7a07c4df6d05d02778027e84ce00a10b9b14/R/table.R#L489
  table_has_caption <- (length(md_table) > 2 && md_table[2] == '')

  if (table_has_caption) {
    table_caption_line <- md_table[1]
    # Well, here is a guess. It will not work if people use custom caption.label
    table_caption <- trimws(sub('Table:', '', table_caption_line))
    md_table <- md_table[3:length(md_table)]
  } else {
    table_caption <- NA
  }

  thead_line <- md_table[1]
  separator_line <- md_table[2]
  tbody_lines <- md_table[3:length(md_table)]

  # Analyze separator line
  cell_indices <- separator_indices(separator_line)
  alignment_raw <- line_separator(separator_line, cell_indices)
  alignment <- sapply(alignment_raw, function(x) {
    if (grepl("^:-+$", x)) 'l' else if (grepl("^:-+:$", x)) 'c' else 'r'
  })

  n_cols <- length(alignment)
  n_rows <- length(tbody_lines)

  # thead and tbody
  # Each line may have different indices if double-width
  # characters are used (issue #821)
  cell_indices <- separator_indices(thead_line)
  header_row <- line_separator(thead_line, cell_indices)
  body_rows <- sapply(tbody_lines, function(line) {
    cell_indices <- separator_indices(line)
    line_separator(line, cell_indices)
  })
  table_matrix <- matrix(body_rows, ncol = n_cols, byrow = TRUE)

  return(kbl(table_matrix, col.names=header_row, align=alignment,
             caption=table_caption, booktabs = TRUE,
             longtable = TRUE))
}

# \toprule, \midrule and \bottomrule have an optional width argument #806
# Match it all.  Sometimes these are used with
# the extended regex language, sometimes with PCRE
# or ICU, so be careful!
toprule_regexp <- "(\\\\toprule(\\[[^]]*])?)"
midrule_regexp <- "(\\\\midrule(\\[[^]]*])?)"
bottomrule_regexp <- "(\\\\bottomrule(\\[[^]]*])?)"

finalize_latex <- function(out, kable_attrs, table_info) {
  kable_attrs$format <- "latex"
  attributes(out) <- kable_attrs
  if (!inherits(out, "knitr_kable"))
    class(out) <- "knitr_kable"
  attr(out, "kable_meta") <- table_info
  out
}
