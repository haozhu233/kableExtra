auto_set_format <- function() {
  if (knitr::is_latex_output()) {
    options(knitr.table.format = "latex")
  } else {
    options(knitr.table.format = "html")
  }
}
