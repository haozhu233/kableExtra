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
add_footnote <- function(input, label = NULL, notation = "alphabet") {
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
    )
  )
  ids <- ids.ops[,notation]

  if(!attr(input, "format") %in% c("html", "latex")){
    warning("Currently kableExtra only supports html and latex. You have to specify your kable export format or set it in the global option `knitr.table.format`.")
    export <- input
    }

  # Generate latex table footnote using threeparttable --------------------------------
  if(attr(input, "format")=="latex"){
    #count the number of items in label and intable notation
    count.label = length(label)
    count.intablenoot = sum(gregexpr("\\[note\\]", input)[[1]]>0)
      if (count.intablenoot != 0 & count.label != count.intablenoot){
        warning(paste("You entered", count.label, "labels but you put",
                      count.intablenoot, "[note] in your table."))
      }

    # generate footer with appropriate symbol
    footer <- ""
    for(i in 1:count.label){
      footer <- paste0(footer,"\\\\item [", ids[i], "] ", label[i], "\n")
    }

    # Replace in-table notation with appropriate symbol
    for(i in 1:count.intablenoot){
      input <- sub("\\[note\\]", paste0("\\\\textsuperscript{", ids[i], "}"), input)
    }

    #
    if(grepl("\\\\caption\\{.*?\\}", input)){
      if(grepl("\\\\begin\\{tabular\\}", input)){
        export <- sub("\\\\caption\\{", "\\\\begin{threeparttable}\n\\\\caption{", input)
        }else{export <- input}
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
  if(attr(input, "format")=="html"){

  }
  return(export)
}
