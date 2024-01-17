.onLoad <- function(libname = find.package("kableExtra"), pkgname = "kableExtra") {
  if (knitr::is_latex_output())
    use_latex_packages()

  # auto_format <- getOption("kableExtra.auto_format", default = FALSE)
  # if (auto_format) auto_set_format()

  output_type <- rmarkdown::metadata$output[1]
  if (is.list(output_type))
    output_type <- names(output_type)

  if (!is.null(output_type) && output_type %in% c(
        "ioslides_presentation", "slidy_presentation",
        "gitbook", "bookdown::gitbook", "radix_article", "radix::radix_article",
        "distill_article", "distill::distill_article"
      )) {
    options(kableExtra.html.bsTable = TRUE)
  }
  if (!is.null(knitr::opts_knit$get("rmarkdown.pandoc.to")) &&
      knitr::opts_knit$get("rmarkdown.pandoc.to") %in% c("epub3", "epub")) {
    options(kableExtra.knit_print.dependency = FALSE)
  }
}
