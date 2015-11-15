#' Add footnote
#'
#' @description Add footnote to your favorite kable output. So far this function
#' only works when you define \code{format} in your kable function or in the
#' global knitr option \code{knitr.table.format}. In latex, we are using the
#' \code{threeparttable} package so you need to import this package in your
#' \code{YAML} header.
#'
#' @param input The direct output of your \code{kable} function or your last
#' \code{kableExtra} function.
#' @param label A vector of footnotes you want to add. You don't need to add
#' notations in your notes.
#' @param notation You can select the format of your footnote notation from
#' "number", "alphabet" and "symbol".
#'
#' @export
add_footnote <- function(input, label = NULL, notation = "alphabet", threeparttable = F) {
  if (is.null(label)){return(input)}
  # Define available id list
  if (!notation %in% c("number", "alphabet", "symbol")){
    warning('Please select your notation within "number", "alphabet" and "symbol". Now add_footnote is using "alphabet" as default.')
  }
  if (notation == "symbol") {notation = paste0(notation, ".", attr(input, "format"))}
  ids.ops <- data.frame(
    number = as.character(1:20),
    alphabet = letters[1:20],
    symbol.latex = c(
      "*", "\\\\dag", "\\\\ddag", "\\\\S", "\\\\P",
      "**", "\\\\dag\\\\dag", "\\\\ddag\\\\ddag", "\\\\S\\\\S", "\\\\P\\\\P",
      "***", "\\\\dag\\\\dag\\\\dag", "\\\\ddag\\\\ddag\\\\ddag", "\\\\S\\\\S\\\\S", "\\\\P\\\\P\\\\P",
      "****", "\\\\dag\\\\dag\\\\dag\\\\dag", "\\\\ddag\\\\ddag\\\\ddag\\\\ddag", "\\\\S\\\\S\\\\S\\\\S", "\\\\P\\\\P\\\\P\\\\P"
    ),
    symbol.html = c(
      "*", "&dagger;", "&Dagger;", "&sect;", "&para;",
      "**", "&dagger;&dagger;", "&Dagger;&Dagger;", "&sect;&sect;", "&para;&para;",
      "*", "&dagger;&dagger;&dagger;", "&Dagger;&Dagger;&Dagger;", "&sect;&sect;&sect;", "&para;&para;&para;",
      "**", "&dagger;&dagger;&dagger;&dagger;", "&Dagger;&Dagger;&Dagger;&Dagger;", "&sect;&sect;&sect;&sect;", "&para;&para;&para;&para;"
    ),
    symbol.markdown = c(
      "\\*", "†", "‡", "§", "¶",
      "\\*\\*", "††", "‡‡", "§§", "¶¶",
      "\\*\\*\\*", "†††", "‡‡‡", "§§§", "¶¶¶",
      "\\*\\*\\*\\*", "††††", "‡‡‡‡", "§§§§", "¶¶¶¶"
    ),
    symbol.pandoc = c(
      "\\*", "†", "‡", "§", "¶",
      "\\*\\*", "††", "‡‡", "§§", "¶¶",
      "\\*\\*\\*", "†††", "‡‡‡", "§§§", "¶¶¶",
      "\\*\\*\\*\\*", "††††", "‡‡‡‡", "§§§§", "¶¶¶¶"
    )
  )
  ids <- ids.ops[,notation]
  ids.intable <- gsub("\\*", "\\\\*", ids)

  #count the number of items in label and intable notation
  count.label = length(label)
  count.intablenoot = sum(str_count(input, "\\[note\\]"))
  if (count.intablenoot != 0 & count.label != count.intablenoot){
    warning(paste("You entered", count.label, "labels but you put",
                  count.intablenoot, "[note] in your table."))
  }

  if(!attr(input, "format") %in% c("html", "latex")){
    export <- input
    if(count.intablenoot != 0){
      for(i in 1:count.intablenoot){
        export[which(str_detect(export, "\\[note\\]"))[1]] <-
          sub("\\[note\\]", paste0("^", ids.intable[i], "^",
            paste0(rep(" ", 4 - nchar(as.character(ids[i]))),
              collapse = "")), export[which(str_detect(export, "\\[note\\]"))[1]])
      }
    }

    export[length(export)+1] <- ""
    export[length(export)+1] <- "__Note:__"
    export[length(export)+1] <- paste0(
      paste0("^", ids[1:length(label)], "^ ", label), collapse = " "
      )
    }

  # Generate latex table footnote --------------------------------
  if(attr(input, "format")=="latex"){
    # If longtable is used, then use page footnote instead of threeparttable
    # as it makes more sense to see the footnote at the bottom of page if
    # table is longer than one page.
    if(grepl("\\\\begin\\{longtable\\}", input)){

      for(i in 1:count.intablenoot){
        input <- sub("\\[note\\]", paste0("\\\\footnote[", ids[i], "]{", label[i], "}"), input)
      }
    }else{
      # Regular cases other than longtable
      # generate footer with appropriate symbol
      footer <- ""
      for(i in 1:count.label){
        footer <- paste0(footer,"\\\\item [", ids[i], "] ", label[i], "\n")
      }

      # Replace in-table notation with appropriate symbol
      for(i in 1:count.intablenoot){
        input <- sub("\\[note\\]", paste0("\\\\textsuperscript{", ids[i], "}"), input)
      }

      if(grepl("\\\\caption\\{.*?\\}", input)){
        export <- sub("\\\\caption\\{", "\\\\begin{threeparttable}\n\\\\caption{", input)
      }else{
        export <- sub("\\\\begin\\{tabular\\}", "\\\\begin{threeparttable}\n\\\\begin{tabular}", input)
      }
      export <- gsub(
        "\\\\end\\{tabular\\}",
        paste0(
          "\\\\end{tabular}\n\\\\begin{tablenotes}\n\\\\small\n",
          footer, "\\\\end{tablenotes}\n\\\\end{threeparttable}"
        ),
        export)
    }
  }
  if(attr(input, "format")=="html"){
    export <- input
  }
  return(export)
}



