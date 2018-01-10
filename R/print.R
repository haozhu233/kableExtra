#' @export
print.kableExtra <- function(x) {
  html_header <- htmltools::tags$head(
    rmarkdown::html_dependency_jquery(),
    rmarkdown::html_dependency_bootstrap(theme = "simplex")
  )

  html_table <- htmltools::HTML(as.character(x))
  cat(as.character(x))
  htmltools::html_print(htmltools::tagList(html_header, html_table))
}


#' @export
knit_print.kableExtra <- function(x) {
  x <- paste0(x, "\n\n")
  asis_output(x)
}




