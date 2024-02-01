#' Remove columns
#'
#' @param kable_input Output of [knitr::kable()] with format specified
#' @param columns A numeric value or vector indicating in which column(s) rows
#' need to be removed
#'
#' @export
#'
#' @examples
#' \dontrun{
#' remove_column(kable(mtcars), 1)
#' }
remove_column <- function (kable_input, columns) {
    if (is.null(columns)) return(kable_input)
    kable_format <- attr(kable_input, "format")
    if (kable_format %in% c("pipe", "markdown")) {
      kable_input <- md_table_parser(kable_input)
      kable_format <- attr(kable_input, "format")
    }
    if (!kable_format %in% c("html", "latex")) {
        warning("Please specify format in kable. kableExtra can customize",
                " either HTML or LaTeX outputs. See ",
                "https://haozhu233.github.io/kableExtra/ for details.")
        return(kable_input)
    }

    columns <- sort(unique(columns))
    if (kable_format == "html") {
        return(remove_column_html(kable_input, columns))
    } else if (kable_format == "latex") {
        stop("Removing columns was not implemented for latex kables yet")
    }
}

remove_column_html <- function (kable_input, columns) {
    kable_attrs <- attributes(kable_input)
    important_nodes <- read_kable_as_xml(kable_input)
    body_node <- important_nodes$body
    kable_xml <- important_nodes$table
    kable_tbody <- xml_tpart(kable_xml, "tbody")
    if (is.null(kable_tbody))
      return(kable_input)
    kable_thead <- xml_tpart(kable_xml, "thead")

    group_header_rows <- attr(kable_input, "group_header_rows")
    all_contents_rows <- seq(1, length(xml_children(kable_tbody)))

    if (!is.null(group_header_rows)) {
        warning("It's recommended to use remove_column after add_header_above.",
                "Right now some column span numbers might not be correct. ")
        all_contents_rows <- all_contents_rows[!all_contents_rows %in%
                                                   group_header_rows]
    }

    collapse_matrix <- attr(kable_input, "collapse_matrix")
    collapse_columns <- NULL
    if (!is.null(collapse_matrix)) {
        collapse_columns <- sort(as.numeric(sub("x", "",
                                                names(collapse_matrix))))
        collapse_columns_origin <- collapse_columns
    }
    while (length(columns) > 0) {
        if (!is.null(kable_thead))
          xml2::xml_remove(xml2::xml_child(
            xml2::xml_child(kable_thead, xml2::xml_length(kable_thead)),
              columns[1]))
        if (length(collapse_columns) != 0 && collapse_columns[1] <= columns[1]){
            if (columns[1] %in% collapse_columns) {
                column_span <- collapse_matrix[[paste0('x', columns[1])]]
                non_skip_rows <- column_span != 0
                collapse_columns <- collapse_columns[
                    collapse_columns != columns[1]
                    ] - 1
            } else {
                non_skip_rows <- rep(TRUE, length(all_contents_rows))
            }
            prior_col <- which(collapse_columns_origin < columns[1])
            for (i in all_contents_rows[non_skip_rows]) {
                if (length(prior_col) == 0) {
                    pos_adj <- 0
                } else {
                    pos_adj <- sum(collapse_matrix[i, prior_col] == 0)
                }
                target_cell <- xml2::xml_child(
                    xml2::xml_child(kable_tbody, i),
                    columns[1] - pos_adj)
                xml2::xml_remove(target_cell)
            }
        } else {
            for (i in all_contents_rows) {
                target_cell <- xml2::xml_child(
                    xml2::xml_child(kable_tbody, i),
                    columns[1])
                xml2::xml_remove(target_cell)
            }
        }
        # not very efficient but for finite task it's probably okay
        columns <- (columns - 1)[-1]
    }
    out <- as_kable_xml(body_node)
    attributes(out) <- kable_attrs
    if (!"kableExtra" %in% class(out))
        class(out) <- c("kableExtra", class(out))

    return(out)
}
