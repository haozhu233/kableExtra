.onLoad <- function(libname = find.package("kableExtra"), pkgname = "kableExtra") {
  if (knitr::is_latex_output()) {
    load_packages <- getOption("kableExtra.latex.load_packages", default = TRUE)
    if (load_packages) {
      usepackage_latex("booktabs")
      usepackage_latex("longtable")
      usepackage_latex("array")
      usepackage_latex("multirow")
      usepackage_latex("wrapfig")
      usepackage_latex("float")
      usepackage_latex("colortbl")
      usepackage_latex("pdflscape")
      usepackage_latex("tabu")
      usepackage_latex("threeparttable")
      usepackage_latex("threeparttablex")
      usepackage_latex("ulem", "normalem")
      usepackage_latex("makecell")
      usepackage_latex("xcolor")
    }
  }
  auto_format <- getOption("kableExtra.auto_format", default = TRUE)
  if (auto_format) auto_set_format()
  if (!is.null(rmarkdown::metadata$output) &&
      names(rmarkdown::metadata$output)[1] %in% c(
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
