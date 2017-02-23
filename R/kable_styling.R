#' HTML table attributes
#'
#' @description This function provides a cleaner approach to modify the style
#' of HTML tables other than using the `table.attr` option in `knitr::kable()`.
#' Currenly, it assumes the HTML document has boot
#'
#' @param bootstrap_options A character vector for bootstrap table options. For
#' detailed information, please check the package vignette or visit the
#' w3schools' \href{https://www.w3schools.com/bootstrap/bootstrap_tables.asp}{Bootstrap Page}
#' . Possible options include "basic", "striped", "bordered", "hover",
#' "condensed" and "responsive".
#' @param full_width A `TRUE` or `FALSE` variable controlling whether the HTML
#' table should have 100\% width.
#' @param float A character string determining whether and how the HTML table
#' should float on the page. Values could be "left", "center", "right"
#' @param font_size A numeric input for table font size
#'
#' @export
kable_styling <- function(kable_input,
                          bootstrap_options = "basic",
                          full_width = T,
                          float = c("center", "left", "right"),
                          font_size = NULL,
                          latex_hold_position = F,
                          latex_scale_down = F) {
  kable_format <- attr(kable_input, "format")
  if (!kable_format %in% c("html", "latex")) {
    stop("Please specify output format in your kable function. Currently ",
         "generic markdown table using pandoc is not supported.")
  }
  if (kable_format == "html") {
    return(htmlTable_styling(kable_input,
                             bootstrap_options = bootstrap_options,
                             full_width = full_width,
                             float = float,
                             font_size = font_size))
  }
  if (kable_format == "latex") {
    return(pdfTable_styling(full_width = full_width,
                            float = float,
                            font_size = font_size,
                            latex_hold_position = latex_hold_position,
                            latex_scale_down = latex_scale_down))
  }
}

# htmlTable Styling ------------
htmlTable_styling <- function(kable_input,
                              bootstrap_options = "basic",
                              full_width = T,
                              float = c("center", "left", "right"),
                              font_size = NULL) {
  kable_xml <- read_xml(as.character(kable_input), options = c("COMPACT"))

  # Modify class
  bootstrap_options <- match.arg(
    bootstrap_options,
    c("basic", "striped", "bordered", "hover", "condensed", "responsive"),
    several.ok = T
  )

  kable_xml_class <- NULL
  if (xml_has_attr(kable_xml, "class")) {
    kable_xml_class <- xml_attr(kable_xml, "class")
  }
  if (length(bootstrap_options) == 1 && bootstrap_options == "basic") {
    bootstrap_options <- "table"
  } else {
    bootstrap_options <- bootstrap_options[bootstrap_options != "basic"]
    bootstrap_options <- paste0("table-", bootstrap_options)
    bootstrap_options <- c("table", bootstrap_options)
  }
  xml_attr(kable_xml, "class") <- paste(c(kable_xml_class, bootstrap_options),
                                        collapse = " ")

  # Modify style
  kable_xml_style <- NULL
  if (xml_has_attr(kable_xml, "style")) {
    kable_xml_style <- xml_attr(kable_xml, "style")
  }
  if (!is.null(font_size)) {
    kable_xml_style <- c(kable_xml_style,
                         paste0("font-size: ", font_size, "px;"))
  }
  if (!full_width) {
    kable_xml_style <- c(kable_xml_style, "width: auto !important;")
  }

  float <- match.arg(float)
  if (float == "center") {
    kable_xml_style <- c(kable_xml_style,
                         "margin-left:auto; margin-right:auto;")
  }
  if (float == "right") {
    kable_xml_style <- c(kable_xml_style,
                         "float: right;")
  }
  if (length(kable_xml_style) != 0) {
    xml_attr(kable_xml, "style") <- paste(kable_xml_style, collapse = " ")
  }
  return(structure(as.character(kable_xml), format = "html",
                   class = "knitr_kable"))
}

# LaTeX table style
pdfTable_styling <- function(kable_input,
                             full_width = T,
                             float = c("center", "left", "right"),
                             font_size = NULL,
                             latex_hold_position = F,
                             latex_scale_down = F) {

}
