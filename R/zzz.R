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
    usepackage_latex("ulem", "normalem")
  }
}
