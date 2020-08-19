#' Rmarkdown Format
#'
#' @description If the export format of the Rmarkdown document exist,
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
  return(x)
}

as_kable_xml <- function(x) {
  out <- structure(as.character(x), format = "html", class = "knitr_kable")
  return(out)
}

read_kable_as_xml <- function(x) {
  kable_html <- read_html(as.character(x), options = c("RECOVER", "NOERROR"))
  xml_child(xml_child(kable_html, 1), 1)
}

#' LaTeX Packages
#' @description This function shows all LaTeX packages that is supposed to be
#' loaded for this package in a rmarkdown yaml format.
#'
#' @export
kableExtra_latex_packages <- function() {

  pkg_list <- paste0("  - ", latex_pkg_list())

  pkg_text <- paste0(
    "header-includes:\n",
    paste0(pkg_list, collapse = "\n")
  )

  cat(pkg_text)
}

latex_pkg_list <- function() {
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
    "\\usepackage[normalem]{ulem}",
    "\\usepackage[utf8]{inputenc}",
    "\\usepackage{makecell}"
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
      paste0("\\\\\\\\vphantom\\\\{", empty_times, "\\\\}\\1"))
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

