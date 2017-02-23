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
#' @export
usepackage_latex <- function(name, options = NULL) {
  invisible(knit_meta_add(list(latex_dependency(name, options))))
}
