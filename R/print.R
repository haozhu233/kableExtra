#' @export
print.kableExtra <- function(x, ...) {
  html_header <- htmltools::tags$head(
    rmarkdown::html_dependency_jquery(),
    rmarkdown::html_dependency_bootstrap(theme = "simplex"),
    html_dependency_kePrint()
  )
  html_table <- htmltools::HTML(as.character(x))
  html_result <- htmltools::tagList(html_header, html_table)
  if (interactive() & rstudioapi::isAvailable()) {
    htmltools::html_print(html_result, viewer = rstudioapi::viewer)
  }
  print(html_result)
}

#' HTML dependency for js script to enable bootstrap tooltip and popup message
#'
#' @export
html_dependency_kePrint <- function() {
  htmlDependency(name = "kePrint",
                 version = "0.0.1",
                 src = system.file("kePrint-0.0.1",
                                   package = "kableExtra"),
                 script = "kePrint.js")
}

#' HTML dependency for Twitter bootstrap (table only)
#'
#' @export
html_dependency_bsTable <- function() {
  htmlDependency(name = "bsTable",
                 version = "3.3.7",
                 src = system.file("bootstrapTable-3.3.7",
                                   package = "kableExtra"),
                 stylesheet = "bootstrapTable.min.css")
}

#' @export
knit_print.kableExtra <- function(x, ...) {
  x <- paste0(x, "\n\n")
  meta_list <- list(html_dependency_kePrint())
  bs <- getOption("kableExtra.html.bsTable", default = FALSE)
  if (bs) {
    meta_list[[2]] <- html_dependency_bsTable()
  }
  asis_output(x, meta = meta_list)
}




