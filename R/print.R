#' @export
print.kableExtra <- function(x, ...) {
  cat(as.character(x))
  html_header <- htmltools::tags$head(
    rmarkdown::html_dependency_jquery(),
    rmarkdown::html_dependency_bootstrap(theme = "simplex"),
    html_dependency_kePrint()
  )
  html_table <- htmltools::HTML(as.character(x))
  htmltools::html_print(htmltools::tagList(html_header, html_table))
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




