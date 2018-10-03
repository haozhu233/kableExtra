.onLoad <- function(libname = find.package("kableExtra"), pkgname = "kableExtra") {
  load_packages <- getOption("kableExtra.latex.load_packages", default = TRUE)
  if (load_packages) {
    usepackage_latex("booktabs")
    usepackage_latex("longtable")
    usepackage_latex("array")
    usepackage_latex("multirow")
    usepackage_latex("xcolor", "table")
    usepackage_latex("wrapfig")
    usepackage_latex("float")
    usepackage_latex("colortbl")
    usepackage_latex("pdflscape")
    usepackage_latex("tabu")
    usepackage_latex("threeparttable")
    usepackage_latex("threeparttablex")
    usepackage_latex("ulem", "normalem")
    usepackage_latex("makecell")
  }
  auto_format <- getOption("kableExtra.auto_format", default = TRUE)
  if (auto_format) auto_set_format()
  if (!is.null(rmarkdown::metadata$output) &&
      rmarkdown::metadata$output %in% c(
        "ioslides_presentation", "slidy_presentation"
      )) {
    options(kableExtra.html.bsTable = TRUE)
  }
  if (!is.null(knitr::opts_knit$get("rmarkdown.pandoc.to")) &&
      knitr::opts_knit$get("rmarkdown.pandoc.to") %in% c("epub3", "epub")) {
    options(kableExtra.knit_print.dependency = FALSE)
  }
}
