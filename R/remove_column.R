#' Remove columns
#'
#' @param kable_input Output of [knitr::kable()] with format specified
#' @param columns A numeric value or vector indicating in which column(s) rows need to be removed
#'
#' @export
#'
#' @examples
#' mtcars %>% 
#' kable() %>% 
#'     remove_column(2:3)
remove_column <- function (kable_input, columns) {
    kable_format <- attr(kable_input, "format")
    if (!kable_format %in% c("html", "latex")) {
        warning("Please specify format in kable. kableExtra can customize either ", 
                "HTML or LaTeX outputs. See https://haozhu233.github.io/kableExtra/ ", 
                "for details.")
        return(kable_input)
    }
    if (kable_format == "html") {
        return(remove_column_html(kable_input, columns))
    } else if (kable_format == "latex") {
        stop("Removing columns was not implemented for latex kables yet")
    }
}

remove_column_html <- function (kable_input, columns) {
    kable_attrs <- attributes(kable_input)
    kable_xml <- kable_as_xml(kable_input)
    kable_tbody <- xml_tpart(kable_xml, "tbody")
    kable_thead <- xml_tpart(kable_xml, "thead")
    
    cell_topleft <- xml2::xml_child(kable_thead, 1) %>% 
        xml2::xml_child(1) %>% 
        xml2::xml_text() %>% 
        stringr::str_trim()
    has_rownames <- cell_topleft==""
    if(has_rownames) columns <- columns+1
    
    body_rows <- xml2::xml_length(kable_tbody)
    for(i in 1:body_rows){
        for(j in columns){
            target_row <- xml2::xml_child(kable_tbody, i)
            target_cell <- xml2::xml_child(target_row, j)
            xml2::xml_remove(target_cell)
        }
    }
    target_row_head <- xml2::xml_child(kable_thead, 1)
    for(j in columns){
        target_cell_head <- xml2::xml_child(target_row_head, j)
        xml2::xml_remove(target_cell_head)
    }
    out <- as_kable_xml(kable_xml)
    attributes(out) <- kable_attrs
    if (!"kableExtra" %in% class(out)) 
        class(out) <- c("kableExtra", class(out))
    
    return(out)
}
