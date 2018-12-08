#' Put a HTML table into a scrollable box
#'
#' @description This function will put a HTML kable object in a fixed-height,
#' fixed-width or both box and make it scrollable.
#'
#' @param kable_input A HTML kable object
#' @param height A character string indicating the height of the box, e.g. "50px"
#' @param width A character string indicating the width of the box, e.g. "100px"
#' @param box_css CSS text for the box
#' @param extra_css Extra CSS styles
#' @param fixed_thead A list of two named element. enabled and background.
#' Default is F and white, e.g. "list(enabled = T, background = "#fff")"
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Specify table size by pixels
#' kable(cbind(mtcars, mtcars), "html") %>%
#'     kable_styling() %>%
#'     scroll_box(width = "500px", height = "200px")
#'
#' # Specify by percent
#' kable(cbind(mtcars, mtcars), "html") %>%
#'     kable_styling() %>%
#'     scroll_box(width = "100%", height = "200px")
#' }

scroll_box <- function(kable_input, height = NULL, width = NULL,
                       box_css = "border: 1px solid #ddd; padding: 5px; ",
                       extra_css = NULL, fixed_thead = list(enabled = F, background = "#fff")) {

  kable_attrs <- attributes(kable_input)

  if (fixed_thead$enabled) {
    box_css = "border: 1px solid #ddd; padding: 0px; "
    kable_xml <- read_kable_as_xml(kable_input)
    kable_thead <- xml_tpart(kable_xml, "thead")
    original_header_row <- xml_child(kable_thead, length(xml_children(kable_thead)))
    for (theader_i in 1:length(xml_children(original_header_row))) {
      target_header_cell <- xml_child(original_header_row, theader_i)
      xml_attr(target_header_cell, "style") <- paste0(xml_attr(target_header_cell, "style"),
                                                      "position: sticky; top:0; background: ",
                                                      fixed_thead$background,";")
    }
    out <- as.character(as_kable_xml(kable_xml))
  } else {
    out <- as.character(kable_input)
  }

  box_styles <- c(box_css, extra_css)

  if (!is.null(height)) {
    box_styles <- c(box_styles,
                    paste0("overflow-y: scroll; height:", height, "; "))
  }

  if (!is.null(width)) {
    box_styles <- c(box_styles,
                    paste0("overflow-x: scroll; width:", width, "; "))
  }

  out <- paste0('<div style="', paste(box_styles, collapse = ""), '">',
                out, '</div>')
  out <- structure(out, format = "html",
                   class = "knitr_kable")
  attributes(out) <- kable_attrs

  if (!"kableExtra" %in% class(out)) class(out) <- c("kableExtra", class(out))

  return(out)
}
