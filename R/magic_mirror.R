#' Magic mirror that returns kable's attributes
#'
#' @param input The output of kable
#'
#' @export

magic_mirror <- function(input){
  if(!"knitr_kable" %in% attr(input, "class")){
    warning("magic_mirror may not be able to produce correct result if the",
            " input table is not rendered by knitr::kable. ")
  }
  kable_format <- attr(input, "format")
  if (kable_format == "latex"){
    kable_info <- magic_mirror_latex(input)
  }
  if (kable_format == "html"){
    kable_info <- magic_mirror_html(input)
  }
  return(kable_info)
}

#' Magic mirror for latex tables
magic_mirror_latex <- function(input){
  kable_info <- list(tabular = NULL, booktabs = NULL, align = NULL,
                     ncol=NULL, nrow=NULL, colnames = NULL, rownames = NULL,
                     caption = NULL, contents = NULL)
  # Tabular
  kable_info$tabular <- ifelse(
    grepl("\\\\begin\\{tabular\\}", input),
    "tabular", "longtable"
  )
  # Booktabs
  kable_info$booktabs <- ifelse(grepl("\\\\toprule", input), TRUE, FALSE)
  # Align
  kable_info$align <- gsub("\\|", "", str_match(
    input, paste0("\\\\begin\\{", kable_info$tabular,"\\}.*\\{(.*?)\\}"))[2])
  # N of columns
  kable_info$ncol <- nchar(kable_info$align)
  # Caption
  kable_info$caption <- str_match(input, "caption\\{(.*?)\\}")[2]
  # N of rows
  kable_info$nrow <- str_count(input, "\\\\\n") -
    # in the dev version (currently as of 11.2015) of knitr, when longtable is
    # enabled, caption is moved inside the tabular environment. As a result,
    # the number of rows should be adjusted.
    ifelse(
      kable_info$tabular == "longtable" & !is.na(kable_info$caption) &
        !str_detect(input, "\\\\begin\\{table\\}\\n\\n\\\\caption"),
      1,0
    )
  # Contents
  kable_info$contents <- str_match_all(input, "\n(.*)\\\\\\\\")[[1]][,2]
  # Column names
  kable_info$colnames <- str_split(kable_info$contents[1], " \\& ")[[1]]
  # Row names
  kable_info$rownames <- str_extract(kable_info$contents, "^[^ &]*")
  return(kable_info)
}
