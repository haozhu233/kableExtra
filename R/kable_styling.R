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
#' @param position A character string determining whether and how the HTML table
#' should float on the page. Values could be "left", "center", "right"
#' @param font_size A numeric input for table font size
#'
#' @export
kable_styling <- function(kable_input,
                          bootstrap_options = "basic",
                          latex_options = "basic",
                          full_width = NULL,
                          position = c("center", "left", "right",
                                       "float_left", "float_right"),
                          font_size = NULL) {
  kable_format <- attr(kable_input, "format")
  if (!kable_format %in% c("html", "latex")) {
    stop("Please specify output format in your kable function. Currently ",
         "generic markdown table using pandoc is not supported.")
  }
  if (kable_format == "html") {
    if (is.null(full_width)) full_width <- T
    return(htmlTable_styling(kable_input,
                             bootstrap_options = bootstrap_options,
                             full_width = full_width,
                             position = position,
                             font_size = font_size))
  }
  if (kable_format == "latex") {
    if (is.null(full_width)) full_width <- F
    return(pdfTable_styling(kable_input,
                            latex_options = latex_options,
                            full_width = full_width,
                            position = position,
                            font_size = font_size))
  }
}

# htmlTable Styling ------------
htmlTable_styling <- function(kable_input,
                              bootstrap_options = "basic",
                              full_width = T,
                              position = c("center", "left", "right",
                                           "float_left", "float_right"),
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

  position <- match.arg(position)
  position_style <- switch(
    position,
    center = "margin-left: auto; margin-right: auto;",
    left = "text-align: right;",
    right = "margin-right: 0; margin-left: auto",
    float_left = "float: left; margin-right: 10px;",
    float_right = "float: right; margin-left: 10px;"
  )
  kable_xml_style <- c(kable_xml_style, position_style)

  if (length(kable_xml_style) != 0) {
    xml_attr(kable_xml, "style") <- paste(kable_xml_style, collapse = " ")
  }
  return(structure(as.character(kable_xml), format = "html",
                   class = "knitr_kable"))
}

# LaTeX table style
pdfTable_styling <- function(kable_input,
                             latex_options = "basic",
                             full_width = F,
                             position = c("center", "left", "right",
                                       "float_left", "float_right"),
                             font_size = NULL) {

  latex_options <- match.arg(
    latex_options,
    c("basic", "striped", "hold_position", "scale_down"),
    several.ok = T
  )

  out = NULL
  out <- as.character(kable_input)
  table_info <- magic_mirror(kable_input)
  valign <- sub("\\[", "\\\\[", table_info$valign)
  valign <- sub("\\]", "\\\\]", valign)
  begin_tabular <- paste0("\\\\begin\\{", table_info$tabular, "\\}", valign)
  end_tabular <- paste0("\\\\end\\{", table_info$tabular, "\\}")


  if ("striped" %in% latex_options) {
    usepackage_latex("xcolor", "table")
    out <- paste0(
      # gray!6 is the same as shadecolor ({RGB}{248, 248, 248}) in pdf_document
      "\\rowcolors{2}{gray!6}{white}\n",
      out,
      "\n\\rowcolors{2}{white}{white}"
    )
  }

  # hold_position is only meaningful in a table environment
  if ("hold_position" %in% latex_options & table_info$table_env) {
    table_env <- "\\\\begin\\{table\\}"
    out <- sub("\\\\begin\\{table\\}", "\\\\begin\\{table\\}[!h]", out)
  }

  if ("scale_down" %in% latex_options | full_width) {
    out <- sub(begin_tabular,
               paste0("\\\\resizebox\\{\\\\textwidth\\}\\{\\!\\}\\{",
                      begin_tabular),
               out)
    out <- sub(end_tabular, paste0(end_tabular, "\\}"), out)
  }

  if (full_width) {
    size_matrix <- sapply(sapply(table_info$contents, str_split, " & "), nchar)
    col_max_length <- apply(size_matrix, 1, max) + 4
    col_ratio <- round(col_max_length / sum(col_max_length) * 0.9, 2)
    col_align <- paste0("p{", col_ratio, "\\\\hsize}")
    col_align <- paste0("{", paste(col_align, collapse = ""), "}")

    out <- sub(paste0(begin_tabular, "\\{[^\\\\n]*\\}"), begin_tabular, out)
    out <- sub(begin_tabular, paste0(begin_tabular, col_align), out)
  }

  position <- match.arg(position)
  if (position == "right") {
    warning("Right a")
  }

  out <- structure(out, format = "latex", class = "knitr_kable")
  return(out)
}
