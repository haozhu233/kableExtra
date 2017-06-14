#' Rmarkdown Format
#'
#' @description If the export format of the Rmarkdown document exist,
#'
#' @importFrom rmarkdown metadata
#'
#' @export

rmd_format <- function(){
  rmd_output_metadata <- metadata$output
  # rmd_fmt <- ifelse(
  #   is.null(rmd_output_metadata),
  #   "markdown", ifelse(
  #   rmd_output_metadata %in% c("html_document", "rmarkdown::html_vignette"),
  #   "html",ifelse(
  #     rmd_output_metadata %in% c("pdf_document", "rmarkdown::tufte_handout"),
  #     "latex", "markdown"
  #   )))
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

collapse_row_matrix <- function(kable_dt, columns)  {
  mapping_matrix <- list()
  for (i in columns) {
    mapping_matrix[[paste0("x", i)]] <- unlist(lapply(
      rle(kable_dt[, i])$length, function(x) {
      c(x, rep(0, x - 1))
      }))
  }
  mapping_matrix <- data.frame(mapping_matrix)
  return(mapping_matrix)
}


