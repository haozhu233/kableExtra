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
  strsplit(x, " \\& ")
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
  return(x)
}

as_kable_xml <- function(x) {
  # tmp <- tempfile(fileext = ".xml")
  # write_xml(x, tmp, options = c("no_declaration", "format_whitespace", "as_html"))
  # out <- read_file(tmp)
  # out <- structure(out, format = "html", class = "knitr_kable")
  out <- structure(as.character(x), format = "html", class = "knitr_kable")
  return(out)
}

read_kable_as_xml <- function(x) {
  kable_html <- read_html(as.character(x))
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
    "\\usepackage[table]{xcolor}",
    "\\usepackage{wrapfig}",
    "\\usepackage{float}",
    "\\usepackage{colortbl}",
    "\\usepackage{pdflscape}",
    "\\usepackage{tabu}",
    "\\usepackage{threeparttable}"
  ))
}

latex_color <- function(color) {
  if (substr(color, 1, 1) != "#") {
    return(paste0("{", color, "}"))
  } else {
    color <- sub("#", "", color)
    if (nchar(color) == 8) color <- substr(color, 1, 6)
    return(paste0("[HTML]{", color, "}"))
  }
}
