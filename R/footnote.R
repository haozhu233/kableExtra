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
add_footnote <- function(input, label = NULL, notation = "alphabet", escape = T,
                         threeparttable = F) {
  if (is.null(label)){return(input)}

  # Define available id list
  if (!notation %in% c("number", "alphabet", "symbol")){
    warning('Please select your notation within "number", "alphabet" and ',
            '"symbol". Now add_footnote is using "alphabet" as default.')
  }
  if (notation == "symbol") {notation = paste0(notation, ".", attr(input, "format"))}
  ids.ops <- data.frame(
    number = as.character(1:20),
    alphabet = letters[1:20],
    symbol.latex = c(
      "*", "\\\\dag", "\\\\ddag", "\\\\S", "\\\\P",
      "**", "\\\\dag\\\\dag", "\\\\ddag\\\\ddag", "\\\\S\\\\S", "\\\\P\\\\P",
      "***", "\\\\dag\\\\dag\\\\dag", "\\\\ddag\\\\ddag\\\\ddag",
      "\\\\S\\\\S\\\\S", "\\\\P\\\\P\\\\P",
      "****", "\\\\dag\\\\dag\\\\dag\\\\dag", "\\\\ddag\\\\ddag\\\\ddag\\\\ddag",
      "\\\\S\\\\S\\\\S\\\\S", "\\\\P\\\\P\\\\P\\\\P"
    ),
    symbol.html = c(
      "*", "&dagger;", "&Dagger;", "&sect;", "&para;",
      "**", "&dagger;&dagger;", "&Dagger;&Dagger;", "&sect;&sect;", "&para;&para;",
      "*", "&dagger;&dagger;&dagger;", "&Dagger;&Dagger;&Dagger;",
      "&sect;&sect;&sect;", "&para;&para;&para;",
      "**", "&dagger;&dagger;&dagger;&dagger;", "&Dagger;&Dagger;&Dagger;&Dagger;",
      "&sect;&sect;&sect;&sect;", "&para;&para;&para;&para;"
    ),
    symbol.markdown = c(
      "\\*", "\u2020", "\u2021", "\u00A7", "\u00B6",
      "\\*\\*", "\u2020\u2020", "\u2021\u2021", "\u00A7\u00A7", "\u00B6\u00B6",
      "\\*\\*\\*", "\u2020\u2020\u2020", "\u2021\u2021\u2021",
      "\u00A7\u00A7\u00A7", "\u00B6\u00B6\u00B6",
      "\\*\\*\\*\\*", "\u2020\u2020\u2020\u2020", "\u2021\u2021\u2021\u2021",
      "\u00A7\u00A7\u00A7\u00A7", "\u00B6\u00B6\u00B6\u00B6"
    ),
    symbol.pandoc = c(
      "\\*", "\u2020", "\u2021", "\u00A7", "\u00B6",
      "\\*\\*", "\u2020\u2020", "\u2021\u2021", "\u00A7\u00A7", "\u00B6\u00B6",
      "\\*\\*\\*", "\u2020\u2020\u2020", "\u2021\u2021\u2021",
      "\u00A7\u00A7\u00A7", "\u00B6\u00B6\u00B6",
      "\\*\\*\\*\\*", "\u2020\u2020\u2020\u2020", "\u2021\u2021\u2021\u2021",
      "\u00A7\u00A7\u00A7\u00A7", "\u00B6\u00B6\u00B6\u00B6"
    )
  )
  ids <- ids.ops[,notation]
  # pandoc cannot recognize ^*^ as * is a special character. We have to use ^\*^
  ids.intable <- gsub("\\*", "\\\\*", ids)
  ids.simple <- c(
    "*", "\u2020", "\u2021", "\u00A7", "\u00B6",
    "**", "\u2020\u2020", "\u2021\u2021", "\u00A7\u00A7", "\u00B6\u00B6",
    "***", "\u2020\u2020\u2020", "\u2021\u2021\u2021",
    "\u00A7\u00A7\u00A7", "\u00B6\u00B6\u00B6",
    "****", "\u2020\u2020\u2020\u2020", "\u2021\u2021\u2021\u2021",
    "\u00A7\u00A7\u00A7\u00A7", "\u00B6\u00B6\u00B6\u00B6"
  )

  #count the number of items in label and intable notation
  count.label = length(label)
  count.intablenoot = sum(str_count(input, "\\[note\\]"))
  if (count.intablenoot != 0 & count.label != count.intablenoot){
    warning(paste("You entered", count.label, "labels but you put",
                  count.intablenoot, "[note] in your table."))
  }

  export <- input

  # Find out if there are any extra in-table notations needed to be corrected
  extra.notation <- as.numeric(
    str_extract(
      str_extract_all(
        paste0(export, collapse = ""), "\\[note[0-9]{1,2}\\]"
      )[[1]],
      "[0-9]{1,2}"))



  # Footnote solution for markdown and pandoc. It is not perfect as
  # markdown doesn't support complex table formats but this solution
  # should be able to satisfy people who don't want to spend extra
  # time to define their `kable` output.
  if(!attr(input, "format") %in% c("html", "latex")){
    # In table notation
    if(count.intablenoot != 0){
      for(i in 1:count.intablenoot){
        export[which(str_detect(export, "\\[note\\]"))[1]] <-
          sub("\\[note\\]", paste0("^", ids.intable[i], "^",
            paste0(rep(" ", 4 - ceiling(i/5)),
              collapse = "")), export[which(str_detect(export, "\\[note\\]"))[1]])
      }
    }
    # Fix extra in table notation
    for(i in extra.notation){
      export <- gsub(paste0("\\[note", i, "\\]"),
                     paste0("^", ids.intable[i], "^",
                            paste0(rep(" ", 4 - ceiling(i/5)),
                            collapse = "")),
                     export)
    }

    export[length(export)+1] <- ""
    export[length(export)+1] <- "__Note:__"
    export[length(export)+1] <- paste0(
      paste0("^", ids[1:length(label)], "^ ", label), collapse = " "
      )
    }

  # Generate latex table footnote --------------------------------
  if(attr(input, "format")=="latex"){
    # Clean the entry for labels when escape is enabled
    if (escape = T){label <- knitr:::escape_latex(label)}

    kable_info <- magic_mirror(input)
    if(kable_info$tabular == "longtable"){
      if(notation != "number"){
        warning("Currently, if you enabled longtable in kable, you can only use",
                " number as your footnote notations. ")
      }
      if(threeparttable == T){
        warning("Currently, threeparttable does not support longtable.")
      }
      # If longtable is used, then use page footnote instead of threeparttable
      # as it makes more sense to see the footnote at the bottom of page if
      # table is longer than one page.

      # Longtable doesn't support footnote in caption directly.
      # See http://tex.stackexchange.com/questions/50151/footnotes-in-longtable-captions
      count.in.caption.note <- str_count(kable_info$caption, "\\[note\\]")
      if (count.in.caption.note != 0){
        # Since caption is the first part of table, we can just start
        caption.footnote <- paste0("\\\\addtocounter{footnote}{-", count.in.caption.note, "}")
        for(i in 1:count.in.caption.note){
          export <- sub("\\[note\\]", "\\\\protect\\\\footnotemark ", export)
          caption.footnote <- paste0(
            caption.footnote, "\n\\\\stepcounter{footnote}\\\\footnotetext{", label[i], "}"
          )
        }

        if (str_detect(export, "\\\\toprule")){
          export <- sub("\\\\toprule",
                        paste0("\\\\toprule\n", caption.footnote), export)
        }else{
          export <- sub("\\\\hline",
                        paste0("\\\\hline\n", caption.footnote), export)
        }
      }
      for(i in (count.in.caption.note + 1):count.intablenoot){
        export <- sub("\\[note\\]",
                      paste0("\\\\footnote[", i, "]{", label[i], "}"), export)
      }
      for(i in extra.notation){
        export <- gsub(paste0("\\[note", i, "\\]"),
                       paste0("\\\\footnotemark[", i, "]"),
                       export)
      }
    }else{
      # Replace in-table notation with appropriate symbol
      for(i in 1:count.intablenoot){
        export <- sub("\\[note\\]", paste0("\\\\textsuperscript{", ids.intable[i], "}"), export)
      }

      # Fix extra in table notation
      for(i in extra.notation){
        export <- gsub(paste0("\\[note", i, "\\]"),
                       paste0("\\\\textsuperscript{", ids.intable[i], "}"),
                       export)
      }
      if(threeparttable == T){
        # generate footer with appropriate symbol
        footer <- ""
        for(i in 1:count.label){
          footer <- paste0(footer,"\\\\item [", ids[i], "] ", label[i], "\n")
        }

        if(grepl("\\\\caption\\{.*?\\}", export)){
          export <- sub("\\\\caption\\{", "\\\\begin{threeparttable}\n\\\\caption{", export)
        }else{
          export <- sub("\\\\begin\\{tabular\\}",
                        "\\\\begin{threeparttable}\n\\\\begin{tabular}", export)
        }
        export <- gsub(
          "\\\\end\\{tabular\\}",
          paste0(
            "\\\\end{tabular}\n\\\\begin{tablenotes}\n\\\\small\n",
            footer, "\\\\end{tablenotes}\n\\\\end{threeparttable}"
          ),
          export)
      }else{
          table.width <- max(nchar(
            str_replace_all(
              str_replace_all(kable_info$contents, "\\[note\\]", ""),
              "\\[note[0-9]{1,2}\\]", ""))) + 2 * (kable_info$ncol - 1)
          footer <- ""
          for (i in 1:count.label){
            label.wrap <- strwrap(label[i], table.width)
            footer <- paste0(footer, "\\\\multicolumn{", kable_info$ncol,
                             "}{l}{\\\\textsuperscript{", ids[i], "} ",
                             label.wrap[1], "}\\\\\\\\\n")
            if(length(label.wrap) > 1){
              for (j in 2:length(label.wrap)){
                footer <- paste0(footer, "\\\\multicolumn{", kable_info$ncol,
                                 "}{l}{", label.wrap[j], "}\\\\\\\\\n")
              }
            }
          }
          export <- gsub("\\\\end\\{tabular\\}",
                         paste0(footer, "\\\\end{tabular}"), export)
      }
    }
  }
  if(attr(input, "format")=="html"){

  }
  return(export)
}
