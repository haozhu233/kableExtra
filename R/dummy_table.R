#' Dummy html table for testing
#' @description Create dummy table for testing in kE
#' @export
dummy_html_tbl <- function() {
  return(kable_styling(kable(mtcars[1:5, 1:5], "html",
                             align = c("l", "l", rep("r", 4)))))
}

#' Dummy latex table for testing
#' @description Create dummy table for testing in kE
#' @export
dummy_latex_tbl <- function(booktabs = T) {
  return(kable_styling(kable(mtcars[1:5, 1:5], "latex", booktabs = booktabs,
                             align = c("l", "l", rep("r", 4)))))
}
