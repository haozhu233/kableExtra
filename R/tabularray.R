# TODO:
# - [ ] HOLD_position does not work unless there's a caption. Where is \begin{table}?
# - [ ] kbl(linesep) can be handled in init_tabularray
# - [ ] longtable -> longtblr
# - [ ] `border_right` and `border_left`


row_spec_tabularray <- function(kable_input,
                                row = NULL,
                                bold = NULL,
                                italic = NULL,
                                monospace = NULL,
                                underline = NULL,
                                strikeout = NULL,
                                color = NULL,
                                background = NULL,
                                align = NULL,
                                font_size = NULL,
                                angle = NULL,
                                hline_after = NULL,
                                extra_latex_after = NULL) {
    out <- kable_input
    rowspec <- attr(out, "tabularray_rowspec")
    linesep <- attr(out, "tabularray_linesep")
    table_info <- magic_mirror(out)


    # sanity checks
    if (!is.null(font_size)) {
        stop("`font_size` is not supported in `row_spec()` with tabularray. Use the `cell_spec()` function instead.", call. = FALSE)
    }
    if (!is.null(angle)) {
        stop("`angle` is not supported in `row_spec()` with tabularray. Use the `cell_spec()` function instead.", call. = FALSE)
    }
    if (!is.null(align)) {
        stop("`align` is not supported in `row_spec()` with tabularray. Use the `cell_spec()` function instead.", call. = FALSE)
    }
    if (!all(row %in% 0:length(rowspec))) {
        msg <- sprintf("`row_spec()` error: column index must be between 1 and %s.", length(rowspec))
        stop(msg, call. = FALSE)
    }

    row <- table_info$position_offset + row

    for (r in row) {
        rowspec[[r]][["fg"]] <- ifelse(is.character(color), color, "")
        rowspec[[r]][["bg"]] <- ifelse(is.character(background), background, "")

        font <- ""
        font <- ifelse(bold, paste0(font, "\\\\bfseries"), font)
        font <- ifelse(monospace, paste0(font, "\\\\ttfamily"), font)
        font <- ifelse(italic, paste0(font, "\\\\itshape"), font)

        cmd <- ""
        cmd <- ifelse(underline, paste0(cmd, "\\\\kableExtraTabularrayUnderline{#1}"), cmd)
        cmd <- ifelse(strikeout, paste0(cmd, "\\\\kableExtraTabularrayStrikeout{#1}"), cmd)

        rowspec[[r]][["font"]] <- font
        rowspec[[r]][["cmd"]] <- cmd

        # clean
        rowspec[[r]] <- lapply(rowspec[[r]], trimws)
        rowspec[[r]] <- Filter(function(x) x != "", rowspec[[r]])
    }

    rowspec_vector <- unlist(lapply(rowspec, make_spec_tabularray))
    # always "" because we use hline{}= instead
    rowspec_string <- sprintf("rowspec={%s},", paste(rowspec_vector, collapse = ""))

    # assumes that rowspec is on its own line
    out <- sub("rowspec=.*", rowspec_string, out, perl = TRUE)

    attr(out, "tabularray_rowspec") <- rowspec

    # priority rowspec over colspec
    at <- attributes(out)
    out <- tmp <- strsplit(out, "\\n")[[1]]
    idx_col <- grep("^colspec=", tmp)[1]
    idx_row <- grep("^rowspec=", tmp)[1]
    out[min(c(idx_col, idx_row))] <- tmp[idx_col]
    out[max(c(idx_col, idx_row))] <- tmp[idx_row]
    out <- paste(out, collapse = "\n")
    for (n in names(at)) {
        attr(out, n) <- at[[n]]
    }
    class(out) <- class(kable_input)

    return(out)
}


column_spec_tabularray <- function(kable_input,
                                   column,
                                   bold,
                                   italic,
                                   monospace,
                                   underline,
                                   strikeout,
                                   color,
                                   background,
                                   width,
                                   latex_valign,
                                   latex_column_spec) {

    out <- kable_input
    vline <- attr(out, "tabularray_vline")
    colspec <- attr(out, "tabularray_colspec")

    # sanity checks
    if (!is.null(latex_column_spec)) {
        stop("latex_column_spec not supported with tabularray", call. = FALSE)
    }

    if (any(!column %in% seq_along(colspec))) {
        msg <- sprintf("`column_spec()` error: `column` must be a vector of unique integers between 1 and %s.", length(colspec))
        stop(msg, call. = FALSE)
    }

    valign <- ifelse(isTRUE(latex_valign == "p"), "m", latex_valign)

    for (col in column) {
        colspec[[col]][["wd"]] <- ifelse(is.character(width), width, "")
        colspec[[col]][["fg"]] <- ifelse(is.character(color), color, "")
        colspec[[col]][["bg"]] <- ifelse(is.character(background), background, "")
        colspec[[col]][["valign"]] <- ifelse(is.character(valign), valign, "")

        font <- ""
        font <- ifelse(bold, paste0(font, "\\\\bfseries"), font)
        font <- ifelse(monospace, paste0(font, "\\\\ttfamily"), font)
        font <- ifelse(italic, paste0(font, "\\\\itshape"), font)

        cmd <- ""
        cmd <- ifelse(underline, paste0(cmd, "\\\\kableExtraTabularrayUnderline{#1}"), cmd)
        cmd <- ifelse(strikeout, paste0(cmd, "\\\\kableExtraTabularrayStrikeout{#1}"), cmd)

        colspec[[col]][["font"]] <- font
        colspec[[col]][["cmd"]] <- cmd

        # clean
        colspec[[col]] <- lapply(colspec[[col]], trimws)
        colspec[[col]] <- Filter(function(x) x != "", colspec[[col]])
    }

    colspec_vector <- unlist(lapply(colspec, make_spec_tabularray))
    colspec_string <- sprintf("colspec={%s},", paste(colspec_vector, collapse = ""))

    # assumes that colspec is on its own line
    out <- sub("colspec=.*", colspec_string, out, perl = TRUE)

    attr(out, "tabularray_colspec") <- colspec

    # priority colspec over rowspec
    at <- attributes(out)
    out <- tmp <- strsplit(out, "\\n")[[1]]
    idx_col <- grep("^colspec=", tmp)[1]
    idx_row <- grep("^rowspec=", tmp)[1]
    out[min(c(idx_col, idx_row))] <- tmp[idx_row]
    out[max(c(idx_col, idx_row))] <- tmp[idx_col]
    out <- paste(out, collapse = "\n")
    for (n in names(at)) {
        attr(out, n) <- at[[n]]
    }
    class(out) <- class(kable_input)

    return(out)
}


cell_spec_tabularray <- function(
    x,
    bold,
    italic,
    monospace,
    underline,
    strikeout,
    color,
    background,
    align,
    font_size,
    angle,
    escape,
    latex_background_in_cell) {

    if (escape) x <- escape_latex(x)

    background <- ifelse(is.null(background), "", paste0("bg=", background))
    color <- ifelse(is.null(color), "", paste0("bg=", color))

    font <- ""
    font <- ifelse(bold, paste0(font, "\\\\bfseries"), font)
    font <- ifelse(monospace, paste0(font, "\\\\ttfamily"), font)
    font <- ifelse(italic, paste0(font, "\\\\itshape"), font)
    if (font != "") font <- paste0("font=", font)

    cmd <- ""
    cmd <- ifelse(underline, paste0(cmd, "\\\\kableExtraTabularrayUnderline{\\#1}"), cmd)
    cmd <- ifelse(strikeout, paste0(cmd, "\\\\kableExtraTabularrayStrikeout{\\#1}"), cmd)
    if (cmd != "") cmd <- paste0("cmd=", cmd)

    if (!is.null(font_size)) {
        x <- paste0(
            "\\bgroup\\fontsize{", font_size, "}{", as.numeric(font_size) + 2,
            "}\\selectfont ", x, "\\egroup{}")
    }
    if (!is.null(angle)) x <- paste0("\\rotatebox{", angle, "}{", x, "}")
    if (!is.null(align)) x <- paste0("\\multicolumn{1}{", align, "}{", x, "}")

    placeholder <- c(background, color, font, cmd)
    placeholder <- Filter(function(x) x != "", placeholder)
    placeholder <- paste(placeholder, collapse = ", ")
    x <- paste0("\\SetCell{", placeholder, "} ", x)
    x <- gsub("\\\\\\\\", "\\\\", x)

    return(x)
}


parse_spec_tabularray <- function(column) {
  # Extracting the arguments within square brackets
  args_str <- gsub("[Q|X]\\[(.*)\\].*", "\\1", column)
  # Splitting the arguments by comma
  args <- trimws(strsplit(args_str, ",")[[1]])
  # Argument list
  keys <- trimws(gsub("=.*", "", args))
  values <- trimws(gsub(".*=", "", args))
  args <- as.list(setNames(values, keys))
  args$type <- ifelse(grepl("^X", column), "X", "Q")
  return(args)
}


make_spec_tabularray <- function(args_list) {
    type <- args_list$type
    args_list$type <- NULL
    args_list <- paste(names(args_list), args_list, sep = "=")
    args_list <- paste(args_list, collapse = ", ")
    out <- sprintf("%s[%s]", type, args_list)
    return(out)
}


init_tabularray <- function(x) {
    cl <- attr(x, "call")
    at <- attributes(x)
    table_info <- magic_mirror(x)
    out <- x

    linesep <- ifelse(is.null(cl$linesep), "", cl$linesep)
    vline <- ifelse(is.null(cl$vline), "", cl$vline)

    # override knitr::kable linesep
    out <- strsplit(out, "\n")[[1]]
    out <- out[out != linesep]
    out <- paste(out, collapse = "\n")

    # extract align from \begin{tblr}[t]{lrcc}
    tab_split <- strsplit(out, "\\n")[[1]]
    idx <- grep("begin\\{tblr\\}", tab_split)
    align <- tab_split[idx]
    align <- gsub(".*\\{(.*)}", "\\1", align)
    align <- strsplit(align, split = "|", fixed = TRUE)[[1]]
    align <- lapply(align, function(x) list(type = "Q", halign = x))
    align <- sapply(align, make_spec_tabularray)

    # basic rows
    rowspec <- rep("Q[]", table_info$nrow + table_info$position_offset)

    # let user specify their own linesep
    rowspec_string <- paste(rowspec, collapse = "")

    tmp <- sprintf(
        "\\begin{tblr}{
colspec={%s},
rowspec={%s},
%s,
%s,
}",
        paste(align, collapse = ""),
        rowspec_string,
        linesep,
        vline
    )

    # cleanup table
    tmp <- strsplit(tmp, "\\n")[[1]]
    tab_split[idx] <- paste(tmp[!tmp %in% c("", ",")], collapse = "\n")

    # prepare new table
    out <- paste(tab_split, collapse = "\n")
    for (n in names(at)) {
        attr(out, n) <- attr(x, n)
    }
    attr(out, "tabularray_colspec") <- lapply(align, parse_spec_tabularray)
    attr(out, "tabularray_rowspec") <- lapply(rowspec, parse_spec_tabularray)
    attr(out, "tabularray_vline") <- vline
    attr(out, "tabularray_linesep") <- linesep
    class(out) <- class(x)

    return(out)
}




# vectorize_style <- function(s, column) {
#     if (is.null(s)) {
#         return(rep("", length(column)))
#     }
#     if (length(s) == 1) {
#         out <- rep(s, length(column))
#     } else {
#         out <- s
#     }
#     if (length(out) != length(column)) {
#         msg <- sprintf("`column_spec()` error: length of style vector must be the same as the length of the `column` index vector.", length(column))
#         stop(msg, .call = FALSE)
#     }
#     return(out)
# }