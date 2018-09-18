#' Make linebreak in LaTeX Table cells
#'
#' @description This function generate LaTeX code of `makecell` so that users
#' can have linebreaks in their table
#'
#' @param x A character vector
#' @param align Choose from "l", "c" or "r"
#' @param double_escape Whether special character should be double escaped.
#' Default is FALSE.
#' @param linebreaker Symbol for linebreaks to replace. Default is `\\n`.
#'
#' @export
linebreak <- function(x, align = c("l", "c", "r"), double_escape = F,
                      linebreaker = "\n") {
  if (is.numeric(x) | is.logical(x)) return(x)
  align <- match.arg(align, c("l", "c", "r"))
  if (double_escape) {
    ifelse(str_detect(x, linebreaker),
           paste0("\\\\makecell[", align, "]{",
                  str_replace_all(x, linebreaker, "\\\\\\\\\\\\\\\\"), "}"),
           x)
  } else {
    ifelse(str_detect(x, linebreaker),
           paste0("\\makecell[", align, "]{",
                  str_replace_all(x, linebreaker, "\\\\\\\\"), "}"),
           x)
  }
}

linebreak_html <- function(x) {
  if (is.numeric(x) | is.logical(x)) return(x)
  ifelse(str_detect(x, "\n"), str_replace_all(x, "\n", "<br />"), x)
}
