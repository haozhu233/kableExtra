#' Put a HTML table into a scrollable box
#'
#' @description This function will put a HTML kable object in a fixed-height,
#' fixed-width or both box and make it scrollable.
#'
#' @param kable_input A HTML kable object
#' @param height A character string indicating the height of the box, e.g. `"50px"`
#' @param width A character string indicating the width of the box, e.g. `"100px"`
#' @param box_css CSS text for the box.  If `height` is specified
#' and `fixed_thead` is `TRUE`, the default is changed to suppress padding.
#' @param extra_css Extra CSS styles
#' @param fixed_thead HTML table option so table header row is fixed at top.
#' Values can be either T/F or `list(enabled = T/F, background = "anycolor")`.
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
                       extra_css = NULL,
                       fixed_thead = TRUE
                       ) {
  kable_format <- attr(kable_input, "format")
  if (kable_format %in% c("pipe", "markdown")) {
    kable_input <- md_table_parser(kable_input)
    kable_format <- attr(kable_input, "format")
  }
  if (kable_format != "html") {
    return(kable_input)
  }
  kable_attrs <- attributes(kable_input)
  fixed_thead <- get_fixed_thead(fixed_thead)
  if (is.null(height)) fixed_thead$enabled <- FALSE

  if (fixed_thead$enabled) {
    if (missing(box_css))
      box_css <- "border: 1px solid #ddd; padding: 0px; "
    important_nodes <- read_kable_as_xml(kable_input)
    body_node <- important_nodes$body
    kable_xml <- important_nodes$table
    all_header_cells <- xml2::xml_find_all(kable_xml, "//thead//th")
    if (is.null(fixed_thead$background))  fixed_thead$background <- "#FFFFFF"
    for (i in seq(length(all_header_cells))) {
      xml_attr(all_header_cells[i], "style") <- paste0(
        xml_attr(all_header_cells[i], "style"),
        "position: sticky; top:0; background-color: ",
        fixed_thead$background, ";"
      )
    }
    out <- as.character(as_kable_xml(body_node))
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

get_fixed_thead <- function(x) {
  if (is.logical(x)) {
    if (x) return(list(enabled = TRUE, background = "#FFFFFF"))
    return(list(enabled = FALSE))
  }
  return(x)
}
