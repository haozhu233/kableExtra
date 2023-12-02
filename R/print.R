#' @export
print.kableExtra <- function(x, ...) {
  view_html <- getOption("kableExtra_view_html", TRUE)
  if (view_html & interactive()) {
    dep <- list(
      rmarkdown::html_dependency_jquery(),
      rmarkdown::html_dependency_bootstrap(theme = "cosmo"),
      html_dependency_kePrint(),
      html_dependency_lightable()
    )
    html_kable <- htmltools::browsable(
      htmltools::HTML(
        as.character(x),
        '<script type="text/x-mathjax-config">MathJax.Hub.Config({tex2jax: {inlineMath: [["$","$"]]}})</script><script async src="https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"></script>'
      )
    )
    htmlDependencies(html_kable) <- dep
    class(html_kable) <- "shiny.tag.list"
    print(html_kable)
  } else {
    cat(as.character(x))
  }
}

#' @export
print.kableExtraInlinePlots <- function(x, ...) {
  view_html <- getOption("kableExtra_view_html", TRUE)
  if (view_html & interactive() & !is.null(x$svg_text)) {
    html_inline <- htmltools::browsable(htmltools::HTML(x$svg_text))
    class(html_inline) <- "shiny.tag.list"
    print(html_inline)
  } else {
    print.simple.list(x)
  }
}

#' HTML dependency for Javascript to enable bootstrap tooltip and popup message
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
                 stylesheet = "bootstrapTable.min.css",
                 script = "bootstrapTable.js")
}

#' HTML dependency for lightable
#'
#' @export
html_dependency_lightable <- function() {
  htmlDependency(name = "lightable",
                 version = "0.0.1",
                 src = system.file("lightable-0.0.1",
                                   package = "kableExtra"),
                 stylesheet = "lightable.css")
}

#' @export
knit_print.kableExtra <- function(x, ...) {
  x <- paste0(x, "\n\n")
  kp_dependency <- getOption("kableExtra.knit_print.dependency",
                             default = TRUE)
  if (kp_dependency) {
    meta_list <- list(html_dependency_kePrint())
    meta_list[[2]] <- html_dependency_lightable()
    bs <- getOption("kableExtra.html.bsTable", default = FALSE)
    if (bs) {
      meta_list[[3]] <- html_dependency_bsTable()
    }
  } else {
    meta_list <- NULL
  }
  asis_output(x, meta = meta_list)
}






