.onLoad <- function(libname = find.package("kableExtra"), pkgname = "kableExtra") {
  usepackage_latex("booktabs")
  usepackage_latex("longtable")
  usepackage_latex("array")
  usepackage_latex("multirow")
  usepackage_latex("xcolor", "table")
  usepackage_latex("wrapfig")
  usepackage_latex("colortbl")
}
