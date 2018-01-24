#' @export
print.kableExtra <- function(x, ...) {
  html_header <- htmltools::tags$head(
    rmarkdown::html_dependency_jquery(),
    rmarkdown::html_dependency_bootstrap(theme = "simplex"),
    html_dependency_kePrint()
  )
  html_table <- htmltools::HTML(as.character(x))
  html_result <- htmltools::tagList(html_header, html_table)
  htmltools::html_print(html_result)

  print(html_result)
}

html_dependency_kePrint <- function() {
  htmlDependency(name = "kePrint",
                 version = "0.0.1",
                 src = system.file("kePrint-0.0.1",
                                   package = "kableExtra"),
                 script = "kePrint.js")
}

#' @export
knit_print.kableExtra <- function(x, ...) {
  x <- paste0(x, "\n\n")
  asis_output(x)
}




