#' Make linebreak in LaTeX Table cells
#'
#' @description This function generates LaTeX code of `makecell` so that users
#' can have linebreaks in their table
#'
#' @param x A character vector
#' @param align Alignment of the text
#' @param double_escape Whether special character should be double escaped.
#' Default is FALSE.
#' @param linebreaker Symbol for linebreaks to replace. Default is `\n`.
#' @details
#' Not all `align` settings are supported, but
#' simple ones such as `l`, `c`, `r` should be fine.
#' More exotic ones like `p{3cm}` should work too,
#' but may not if they are too complicated.
#'
#'
#' @export
linebreak <- function(x, align = "l", double_escape = F,
                      linebreaker = "\n") {
  if (is.numeric(x) | is.logical(x)) return(x)
  x <- as.character(x)

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
