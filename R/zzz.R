.onLoad <- function(libname = find.package("kableExtra"), pkgname = "kableExtra") {
  usepackage_latex("booktabs")
  usepackage_latex("longtable")
  message("LaTeX package booktabs and longtable will be loaded by default.")
}
