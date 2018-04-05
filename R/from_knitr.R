# These functions are imported from knitr/highr as `:::` is not recommended by
# CRAN

# escape special LaTeX characters
# @author Yihui Xie
escape_latex <- function(x, newlines = FALSE, spaces = FALSE) {
  x = gsub('\\\\', '\\\\textbackslash', x)
  x = gsub('([#$%&_{}])', '\\\\\\1', x)
  x = gsub('\\\\textbackslash', '\\\\textbackslash{}', x)
  x = gsub('~', '\\\\textasciitilde{}', x)
  x = gsub('\\^', '\\\\textasciicircum{}', x)
  if (newlines) x = gsub('(?<!\n)\n(?!\n)', '\\\\\\\\', x, perl = TRUE)
  if (spaces) x = gsub('  ', '\\\\ \\\\ ', x)
  x
}

escape_latex2 <- function(x) {
  x = gsub('\\\\', '\\\\\\\\textbackslash', x)
  x = gsub('([#$%&_{}])', '\\\\\\\\\\1', x)
  x = gsub('\\\\textbackslash', '\\\\\\\\textbackslash{}', x)
  x = gsub('~', '\\\\\\\\textasciitilde{}', x)
  x = gsub('\\^', '\\\\\\\\textasciicircum{}', x)
  x
}

# escape special HTML characters
# @author Yihui Xie
# Added conversion
escape_html <- function(x) {
  x = gsub('&', '&amp;', x)
  x = gsub('<', '&lt;', x)
  x = gsub('>', '&gt;', x)
  x = gsub('"', '&quot;', x)
  x = gsub('\n', '<br />', x)
  x
}
