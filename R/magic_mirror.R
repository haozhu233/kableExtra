#' Magic mirror that returns kable's attributes
#'
#' @param input The output of kable
#'
#' @export

magic_mirror <- function(input){
  if(!"knitr_kable" %in% attr(input, "format")){
    warning("magic_mirror may not be able to produce correct result if the",
            " input table is not rendered by knitr::kable. ")
  }
  kable_format <- attr(input, "format")
  if (kable_format == "latex"){
    magic_mirror_latex(input)
  }
  if (kable_format == "html"){
    magic_mirror_html(input)
  }
}

#' Magic mirror for latex tables
magic_mirror_latex <- function(input){
  # kable will put a begin{table} shell if caption is not NULL
  caption <- ifelse(
    str_detect(input, "\\\\caption\\{.*?\\}"),
    str_match(input, "caption\\{(.*?)\\}")[2], NULL
  )
}
