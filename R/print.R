#' @export
print.kableExtra <- function(x, ...) {
  dep <- list(
    rmarkdown::html_dependency_jquery(),
    rmarkdown::html_dependency_bootstrap(theme = "cosmo"),
    html_dependency_kePrint()
  )
  html_kable <- htmltools::browsable(
    htmltools::HTML(as.character(x))
  )
  htmlDependencies(html_kable) <- dep
  class(html_kable) <- "shiny.tag.list"
  print(html_kable)
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
  kp_dependency <- getOption("kableExtra.knit_print.dependency",
                             default = TRUE)
  if (kp_dependency) {
    meta_list <- list(html_dependency_kePrint())
    bs <- getOption("kableExtra.html.bsTable", default = FALSE)
    if (bs) {
      meta_list[[2]] <- html_dependency_bsTable()
    }
  } else {
    meta_list <- NULL
  }
  asis_output(x, meta = meta_list)
}






