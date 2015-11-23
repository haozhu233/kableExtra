#' Rmarkdown Format
#'
#' @description If the export format of the Rmarkdown document exist,
#'
#' @importFrom rmarkdown metadata
#'
#' @export

rmd_format <- function(){
  rmd_output_metadata <- metadata$output
  rmd_fmt <- ifelse(
    is.null(rmd_output_metadata),
    "markdown", ifelse(
    rmd_output_metadata %in% c("html_document", "rmarkdown::html_vignette"),
    "html",ifelse(
      rmd_output_metadata %in% c("pdf_document", "rmarkdown::tufte_handout"),
      "latex", "markdown"
    )))
  return(rmd_fmt)
}
