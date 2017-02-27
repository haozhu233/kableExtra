#' Magic mirror that returns kable's attributes
#'
#' @description Mirror mirror tell me, how does this kable look like?
#'
#' @param kable_input The output of kable
#' @export

magic_mirror <- function(kable_input){
  if (!"knitr_kable" %in% attr(kable_input, "class")) {
    warning("magic_mirror may not be able to produce correct result if the",
            " input table is not rendered by knitr::kable. ")
  }
  if ("original_kable_meta" %in% names(attributes(kable_input))) {
    return(attr(kable_input, "original_kable_meta"))
  }
  kable_format <- attr(kable_input, "format")
  if (kable_format == "latex") {
    kable_info <- magic_mirror_latex(kable_input)
  }
  if (kable_format == "html") {
    kable_info <- magic_mirror_html(kable_input)
  }
  return(kable_info)
}

#' Magic mirror for latex tables --------------
#' @param kable_input The output of kable
magic_mirror_latex <- function(kable_input){
  kable_info <- list(tabular = NULL, booktabs = FALSE, align = NULL,
                     valign = NULL, ncol = NULL, nrow = NULL, colnames = NULL,
                     rownames = NULL, caption = NULL, contents = NULL,
                     centering = FALSE, table_env = FALSE)
  # Tabular
  kable_info$tabular <- ifelse(
    grepl("\\\\begin\\{tabular\\}", kable_input),
    "tabular", "longtable"
  )
  # Booktabs
  kable_info$booktabs <- grepl("\\\\toprule", kable_input)
  # Align
  kable_info$align <- gsub("\\|", "", str_match(
    kable_input, paste0("\\\\begin\\{", kable_info$tabular,"\\}.*\\{(.*?)\\}"))[2])
  # valign
  kable_info$valign <- gsub("\\|", "", str_match(
    kable_input, paste0("\\\\begin\\{", kable_info$tabular,"\\}(.*)\\{.*?\\}"))[2])
  # N of columns
  kable_info$ncol <- nchar(kable_info$align)
  # Caption
  kable_info$caption <- str_match(kable_input, "caption\\{(.*?)\\n")[2]
  # N of rows
  kable_info$nrow <- str_count(kable_input, "\\\\\n") -
    # in the dev version (currently as of 11.2015) of knitr, when longtable is
    # enabled, caption is moved inside the tabular environment. As a result,
    # the number of rows should be adjusted.
    ifelse(
      kable_info$tabular == "longtable" & !is.na(kable_info$caption) &
        !str_detect(kable_input, "\\\\begin\\{table\\}\\n\\n\\\\caption"),
      1,0
    )
  # Contents
  kable_info$contents <- str_match_all(kable_input, "\n(.*)\\\\\\\\")[[1]][,2]
  if (kable_info$tabular == "longtable" & !is.na(kable_info$caption)) {
    kable_info$contents <- kable_info$contents[-1]
  }
  # Column names
  kable_info$colnames <- str_split(kable_info$contents[1], " \\& ")[[1]]
  # Row names
  kable_info$rownames <- str_extract(kable_info$contents, "^[^ &]*")

  kable_info$centering <- grepl("\\\\centering", kable_input)

  kable_info$table_env <- (!is.na(kable_info$caption) &
                             kable_info$tabular != "longtable")
  return(kable_info)
}

#' Magic Mirror for html table --------
#'
#' @param kable_input The output of kable
magic_mirror_html <- function(kable_input){
  kable_info <- list()
  kable_xml <- read_xml(as.character(kable_input))
  # Caption
  kable_info$caption <- xml_text(xml_child(kable_xml, "caption"))
  # Contents
  kable_info$contents <- html_table(read_html(as.character(kable_input)))[[1]]
  # colnames
  kable_info$colnames <- lapply(xml_children(xml_child(kable_xml, "thead")),
                                xml_children)
  kable_info$colnames <- kable_info$colnames[[length(kable_info$colnames)]]
  kable_info$colnames <- trimws(xml_text(kable_info$colnames))
  kable_info$ncol <- length(kable_info$colnames)
  kable_info$nrow_header <- length(xml_children(xml_child(kable_xml, "thead")))
  kable_info$nrow_body <- nrow(kable_info$contents)
  kable_info$table_class <- xml_attr(kable_xml, "class")
  kable_info$table_style <- xml_attr(kable_xml, "style")
  return(kable_info)
}


