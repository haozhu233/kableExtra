#' Separate table headers and add additional header rows based on grouping
#'
#' @description When you create a summary table for either model or basic
#' summary stats in R, you usually end up having column names in the form of
#' "a_mean", "a_sd", "b_mean" and "b_sd". This function streamlines the process
#' of renaming these column names and adding extra header rows using
#' `add_header_above`.
#'
#' @param kable_input Output of `knitr::kable()` with `format` specified
#' @param sep A regular expression separator between groups. The default value
#' is a regular expression that matches any sequence of non-alphanumeric values.
#' @param ... Additional parameters passed to do.call.
#'
#' @export
header_separate <- function(kable_input, sep = "[^[:alnum:]]+", ...) {
  kable_format <- attr(kable_input, "format")
  if (kable_format %in% c("pipe", "markdown")) {
    kable_input <- md_table_parser(kable_input)
    kable_format <- attr(kable_input, "format")
  }
  if (!kable_format %in% c("html", "latex")) {
    warning("Please specify format in kable. kableExtra can customize either ",
            "HTML or LaTeX outputs. See https://haozhu233.github.io/kableExtra/ ",
            "for details.")
    return(kable_input)
  }
  if (kable_format == "html") {
    return(do.call(header_separate_html, list(
      kable_input = kable_input,
      sep = sep,
      ...
    )))
  }
  if (kable_format == "latex") {
    return(do.call(header_separate_latex, list(
      kable_input = kable_input,
      sep = sep,
      ...
    )))
  }
}

header_separate_html <- function(kable_input, sep, ...) {
  kable_attrs <- attributes(kable_input)
  important_nodes <- read_kable_as_xml(kable_input)
  body_node <- important_nodes$body
  kable_xml <- important_nodes$table

  kable_thead <- xml_tpart(kable_xml, "thead")
  if (is.null(kable_thead))
    return(kable_input)

  thead_depth <- length(xml_children(kable_thead))

  if (thead_depth > 1) {
    warning("Your table already has more than 1 rows of thead. header_separate ",
            "won't work in this case and is returning the original input. ")
    return(kable_input)
  }

  original_header_row <- xml_child(kable_thead, thead_depth)
  original_header_cells <- lapply(
    xml_children(original_header_row),
    function(x) trimws(as.character(xml2::xml_contents(x)))
  )

  header_sep <- stringr::str_split(original_header_cells, sep)
  header_layers <- process_header_sep(header_sep)
  new_header_row_one <- lapply(header_layers[[1]], function(x) {
    paste0("<th>", x, "</th>")
  })

  # Fix the original header row
  for (i in seq(length(header_sep))) {
    new_header_row_one[[i]] <- xml2::read_html(new_header_row_one[[i]])
    xml2::xml_attrs(new_header_row_one[[i]]) <-
      xml2::xml_attrs(xml_child(original_header_row, i))
    xml2::xml_replace(xml_child(original_header_row, i),
                      new_header_row_one[[i]])
  }

  out <- as_kable_xml(body_node)
  attributes(out) <- kable_attrs
  if (!"kableExtra" %in% class(out)) class(out) <- c("kableExtra", class(out))

  for (l in seq(2, length(header_layers))) {
    out <- do.call(
      kableExtra::add_header_above,
      list(
        kable_input = out,
        kableExtra::auto_index(header_layers[[l]]),
        ...
      )
    )
  }
  return(out)
}

process_header_sep <- function(header_sep) {
  max_depth <- max(unlist(lapply(header_sep, length)))
  header_layers <- list()
  for (i in seq(max_depth)) {
    header_layers[[i]] <- list()
    for (j in seq(1, length(header_sep))) {
      layer_length <- length(header_sep[[j]])
      if (layer_length > 0) {
        header_layers[[i]][[j]] <- header_sep[[j]][layer_length]
        header_sep[[j]] <- header_sep[[j]][-layer_length]
      } else {
        header_layers[[i]][[j]] <- " "
      }
    }
  }
  header_layers <- lapply(header_layers, unlist)
  return(header_layers)
}

header_separate_latex <- function(kable_input, sep, ...) {
  kable_attrs <- attributes(kable_input)
  table_info <- magic_mirror(kable_input)
  out <- solve_enc(kable_input)

  if (table_info$duplicated_rows) {
    dup_fx_out <- fix_duplicated_rows_latex(out, table_info)
    out <- dup_fx_out[[1]]
    table_info <- dup_fx_out[[2]]
  }

  if (!is.null(table_info$new_header_row)) {
    warning("Your table already has more than 1 rows of thead. header_separate ",
            "won't work in this case and is returning the original input. ")
    return(kable_input)
  }

  original_header_cells <- str_split(table_info$contents[1], " & ")[[1]]

  header_sep <- stringr::str_split(original_header_cells, sep)
  header_layers <- process_header_sep(header_sep)

  # Fix the original header row
  new_header_row_one <- paste0(header_layers[[1]], collapse = ' & ')

  out <- stringr::str_replace(out, paste0(table_info$contents[1], "\\\\\\\\"),
                              paste0(new_header_row_one, "\\\\\\\\"))
  table_info$contents[1] <- new_header_row_one

  out <- finalize_latex(out, kable_attrs, table_info)

  for (l in seq(2, length(header_layers))) {
    out <- do.call(
      kableExtra::add_header_above,
      list(
        kable_input = out,
        kableExtra::auto_index(header_layers[[l]]),
        ...
      )
    )
  }

  return(out)
}


