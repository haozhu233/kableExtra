#' Magic mirror that returns kable's attributes
#'
#' @description Mirror mirror tell me, how does this kable look like?
#'
#' @param kable_input The output of kable
#'
#' @examples magic_mirror(knitr::kable(head(mtcars), "html"))
#' @export

magic_mirror <- function(kable_input){
  if (!"knitr_kable" %in% attr(kable_input, "class")) {
    warning("magic_mirror may not be able to produce correct result if the",
            " input table is not rendered by knitr::kable. ")
  }
  if ("kable_meta" %in% names(attributes(kable_input))) {
    return(attr(kable_input, "kable_meta"))
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

# Magic mirror for latex tables --------------
magic_mirror_latex <- function(kable_input){
  kable_info <- list(tabular = NULL, booktabs = FALSE, align = NULL,
                     valign = NULL, ncol = NULL, nrow = NULL, colnames = NULL,
                     rownames = NULL, caption = NULL, caption.short = NULL,
                     contents = NULL,
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
    kable_input, paste0("\\\\begin\\{",
                        kable_info$tabular,"\\}.*\\{(.*?)\\}"))[2])
  kable_info$align_vector <- unlist(strsplit(kable_info$align, ""))
  kable_info$align_vector_origin <- kable_info$align_vector
  # valign
  kable_info$valign <- gsub("\\|", "", str_match(
    kable_input, paste0("\\\\begin\\{", kable_info$tabular,"\\}(.*)\\{.*?\\}"))[2])
  kable_info$valign2 <- sub("\\[", "\\\\[", kable_info$valign)
  kable_info$valign2 <- sub("\\]", "\\\\]", kable_info$valign2)
  kable_info$valign3 <- sub("\\[", "", kable_info$valign)
  kable_info$valign3 <- sub("\\]", "", kable_info$valign3)
  kable_info$begin_tabular <- paste0("\\\\begin\\{", kable_info$tabular, "\\}",
                                     kable_info$valign2)
  kable_info$end_tabular <- paste0("\\\\end\\{", kable_info$tabular, "\\}")
  # N of columns
  kable_info$ncol <- nchar(kable_info$align)
  # Caption
  if (str_detect(kable_input, "caption\\[")) {
    caption_line <- str_match(kable_input, "\\\\caption(.*)\\n")[2]
    kable_info$caption.short <- str_match(caption_line, "\\[(.*?)\\]")[2]
    kable_info$caption <- substr(caption_line,
                                 nchar(kable_info$caption.short) + 4,
                                 nchar(caption_line))
  } else {
    kable_info$caption <- str_match(kable_input, "caption\\{(.*?)\\n")[2]
  }
  if (kable_info$tabular == "longtable") {
    kable_info$caption <- str_sub(kable_info$caption, 1, -4)
  } else {
    kable_info$caption <- str_sub(kable_info$caption, 1, -2)
  }
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
  kable_info$contents <- regex_escape(kable_info$contents, T)
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

  # just as placeholders that can presumably be filled with conditions,
  # depending on the calls to the various functions.
  kable_info$striped <- FALSE

  kable_info$hold_position <- FALSE

  kable_info$HOLD_position <- FALSE

  kable_info$scale_down <- FALSE

  # this chunk may include all the other repeat_header options that have
  # recently been included
  kable_info$repeat_header <- FALSE

  return(kable_info)
}

# Magic Mirror for html table --------
magic_mirror_html <- function(kable_input){
  kable_info <- list()
  kable_xml <- read_kable_as_xml(kable_input)
  # Caption
  kable_info$caption <- xml_text(xml_child(kable_xml, "caption"))
  # Contents
  # kable_info$contents <- html_table(read_html(as.character(kable_input)))[[1]]
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

reassemble_kable <- function(mirror){
  # conceptually my idea was to redesign all the exported functions or actually
  # their child-functions. These functions would then merely manage the
  # modifications but don't apply them to the kable. Instead they would assign
  # them to the magic mirror (really great concept btw, I like that!), modifying
  # the pre-set defaults therein. This magic-mirror object would then be passed
  # to reassemble_kable, where the respective modifications are put together.
  # Here, an ordinary if-structure (as I showed below) would be sufficient to
  # manage reassembling the kable for each exported functions independently of
  # the other functions that are set (or not). As the reassemble-function will
  # get very long when all the functions are include, it might make sense to
  # write again child-functions, for instance 'styling_latex_font_size' to
  # manage the respective font sizes, etc. So basically, the only change I
  # suggest is to use the magic_mirror object to store all modifications by the
  # exported parent functions, each of which call the reassemble functions,
  # which in turn calls its child-functions. These could, for the sake of
  # clarity, be relabeled 'raLatexFontSize' or so, to indicate that they are
  # child functions to the ReAssemble function. I assume that you have rather
  # similar ideas and I hope it does not sound as if I want to lecture you! But
  # maybe some of it sounds useful to you.

  contents <- mirror$contents

  contents <- gsub("\\\\", "\\", contents, fixed = TRUE)
  contents <- gsub("\\$", "$", contents, fixed = TRUE)
  contents <- gsub("\\{", "{", contents, fixed = TRUE)
  contents <- gsub("\\}", "}", contents, fixed = TRUE)
  contents <- gsub("\\(", "(", contents, fixed = TRUE)
  contents <- gsub("\\)", ")", contents, fixed = TRUE)
  contents <- gsub("\\[", "[", contents, fixed = TRUE)
  contents <- gsub("\\]", "]", contents, fixed = TRUE)
  contents <- gsub("\\*", "*", contents, fixed = TRUE)
  contents <- gsub("\\+", "+", contents, fixed = TRUE)

  mirror$contents <- contents

  header <- paste0("\\toprule\n", mirror$contents[1], "\\\\\n\\midrule\n")
  body <- paste0(mirror$contents[-1], collapse = "\\\\\n")

  if(mirror$table_env){

    # also need to make sure that only one of the following options is set.
    if(mirror$hold_position){
      open_table <- c("\\begin{table}[!h]")
    } else if(mirror$HOLD_position){
      open_table <- c("\\begin{table}[H]")
    } else{
      open_table <- c("\\begin{table}")
    }

    if(mirror$centering){
      cent <- paste0("\n\\centering\n")
    } else{
      cent <- paste0("\n")
    }

    caption <- paste0("\\caption[", mirror$caption.short, "]{", mirror$caption, "}")

    open_table <- paste0(open_table, "\n\n", caption, cent)

    open_tabular <- paste0("\\begin{", mirror$tabular, "}[", mirror$valign3, "]{", mirror$align, "}\n")
    end <- paste0("\\\\\n\\bottomrule\n\\end{tabular}\n\\end{table}\n")

    out <- paste0(open_table, open_tabular, header, body, end)
  } else{
    begin <- paste0("\n\\begin{", mirror$tabular, "}[", mirror$valign3, "]{", mirror$align, "}\n")
    caption <- paste0("\\caption[", mirror$caption.short, "]{", mirror$caption, "}\\\\\n")
    end <- paste0("\\\\\n\\bottomrule\n", "\\end{", mirror$tabular, "}")

    if(mirror$repeat_header){
      # I did not include all the repeat_header options here (yet), because I
      # did not need them (yet), but I assume this would be the place to process
      # them.
      # I prefer the short caption in the repeat-header, but this might be an optional
      # feature.
      continue_caption <- paste0("\\endfirsthead\n\\caption[]{", mirror$caption.short, " \\textit{(continued)}}\\\\\n")
      continue_header <- paste0(header, "\\endhead\n\\endfoot\n\\bottomrule\n\\endlastfoot\n")
      out <- paste0(begin, caption, header, continue_caption, continue_header, body, end)
    } else{
      out <- paste0(begin, caption, header, body, end)
    }

  }


  return(out)
}
