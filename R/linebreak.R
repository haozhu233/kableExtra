#' Make linebreak in LaTeX Table cells
#'
#' @export
linebreak <- function(x, align = c("l", "c", "r"), double_escape = F) {
  if (is.numeric(x) | is.logical(x)) return(x)
  align <- match.arg(align, c("l", "c", "r"))
  if (double_escape) {
    ifelse(str_detect(x, "\n"),
           paste0("\\\\makecell[", align, "]{",
                  str_replace_all(x, "\n", "\\\\\\\\\\\\\\\\"), "}"),
           x)
  } else {
    ifelse(str_detect(x, "\n"),
           paste0("\\makecell[", align, "]{",
                  str_replace_all(x, "\n", "\\\\\\\\"), "}"),
           x)
  }
}
