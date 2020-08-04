
#' Add borders above or below a kable row
#'
#' @param kable_input Output of [knitr::kable()] with format specified
#' @param rows A numeric value or vector indicating in which rows need a border. The header counts as row #1.
#' @param position one of "bottom" or "top"
#' @param thickness a value in px, or one of "thin", "medium", or "thick"
#' @param color the name of a color, a rgb value, a hex code...
#' @param style one of "dotted", "dashed", "solid", "double", "groove", "ridge", "inset", or "outset"
#' @param collapsed the behavior of the border when cells are collapsed. See details.
#' 
#' @section Collapsed cells:
#' If some cells are collapsed over several rows, the behavior is handled by the `collapsed` argument. For instance, lets consider a kable with a cell spanning over rows 3 to 5. If `collapsed="first"`, this cell will have a border for `rows=3`, if `collapsed="last"`, for `rows=5`, and if `collapsed="any"`, for any value between 3 and 5.
#'
#' @seealso [border CSS documentation](https://developer.mozilla.org/fr/docs/Web/CSS/border)
#' @return
#' @export
#'
#' @examples
row_add_border <- function(kable_input, rows, position=c("bottom", "top"), thickness="2px", 
                           color="black", style="solid", collapsed=c("last", "first", "any")) {
    kable_format <- attr(kable_input, "format")
    if (!kable_format %in% c("html", "latex")) {
        warning("Please specify format in kable. kableExtra can customize either ", 
                "HTML or LaTeX outputs. See https://haozhu233.github.io/kableExtra/ ", 
                "for details.")
        return(kable_input)
    }
    if (kable_format == "html") {
        return(row_add_border_html(kable_input, columns))
    } else if (kable_format == "latex") {
        stop("Adding borders was not implemented for latex kables yet")
    }
}


row_add_border_html <- function(kable_input, rows, position=c("bottom", "top"), thickness="2px", 
                                color="black", style="solid", collapsed=c("last", "first", "any")){
    
    collapsed <- match.arg(collapsed)
    position <- match.arg(position)
    extra_css <- glue("border-{position}:{thickness} {style} {color};")
    
    kable_attrs <- attributes(kable_input)
    kable_xml <- kable_as_xml(kable_input)
    kable_tbody <- xml_tpart(kable_xml, "tbody")
    kable_thead <- xml_tpart(kable_xml, "thead")
    
    
    if(1 %in% rows){
        x <- xml2::xml_child(kable_thead, 1)
        current_css <- xml2::xml_attr(x, "style")
        if(is.na(current_css)) current_css<-NULL
        xml2::xml_attr(x, "style") <- paste0(current_css, extra_css)
    }
    
    ncols <- xml2::xml_length(xml2::xml_children(kable_thead))
    collapse_matrix <- get_collapse_matrix(kable_tbody, ncols)
    
    for(i in rows-1){
        if(i==0) next
        target_row <- xml2::xml_child(kable_tbody, i)
        for(j in seq(ncols)){
            if(collapsed!="first" && collapse_matrix[i,j]>1){ #TODO collapsed=="none"
                next
            } else if(collapse_matrix[i,j]==0){
                if(collapsed=="last" && i==ncol(collapse_matrix) || collapse_matrix[i+1,j]!=0){
                    i_up <- which(collapse_matrix[1:i,j]>0)
                    i_up <- max(i_up, na.rm=TRUE)
                    target_row_up <- xml2::xml_child(kable_tbody, i_up)
                    target_cell <- xml2::xml_child(target_row_up, j)
                } else if(collapsed=="any") {
                    i_up <- which(collapse_matrix[1:i,j]>0)
                    i_up <- max(i_up, na.rm=TRUE)
                    target_row_up <- xml2::xml_child(kable_tbody, i_up)
                    target_cell <- xml2::xml_child(target_row_up, j)
                } else {
                    next
                }
            } else {
                j_real<-collapse_matrix[i,]
                j_real[j_real>0] <- 1:sum(j_real>0)
                target_cell <- xml2::xml_child(target_row, j_real[j])
            }
            current_css <- xml2::xml_attr(target_cell, "style")
            if(is.na(current_css)) current_css<-NULL
            xml2::xml_attr(target_cell, "style") <- paste0(current_css, extra_css)
        }
    }    
    
    out <- kableExtra:::as_kable_xml(kable_xml)
    attributes(out) <- kable_attrs
    if (!"kableExtra" %in% class(out)) 
        class(out) <- c("kableExtra", class(out))
        
    return(out)
}


get_collapse_matrix = function(kable_tbody, ncols){
    body_nrows <- xml2::xml_length(kable_tbody)
    out = matrix(1, nrow = body_nrows, ncol=ncols)
    for(i in 1:body_nrows){
        target_row <- xml2::xml_child(kable_tbody, i)
        target_ncols <- xml2::xml_length(target_row)
        for(j in 1:target_ncols){
            target_cell <- xml2::xml_child(target_row, j)
            span = as.numeric(xml2::xml_attr(target_cell, "rowspan")) %>% replace_na(0)
            if(span>0){
                out[i,j]=span
                out[i+seq(from=1, to=span-1),j]=0
            }
        }
    }    
    return(out)
}
