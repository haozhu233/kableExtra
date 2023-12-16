row_spec_tabularray <- function(kable_input,
                                row = NULL,
                                bold = FALSE,
                                italic = FALSE,
                                monospace = FALSE,
                                underline = FALSE,
                                strikeout = FALSE,
                                color = NULL,
                                background = NULL,
                                align = NULL,
                                font_size = NULL,
                                angle = NULL,
                                hline_after = NULL,
                                extra_latex_after = NULL) {

    table_info <- magic_mirror(kable_input)
    rowspec <- table_info$tabularray$rowspec
    out <- kable_input

    # assertions
    # the reason why we force arguments to be scalar is that that vector
    # arguments are fundamentally at odds with the design philosophy of
    # tabularray, which separates style and data, and applies styles to complete
    # rows and columns. it makes more sense to return an informative pointing
    # users to cell_spec(), which can easily deal with the same use case.
    if (!all(sapply(list(font_size, angle, align), is.null))) {
        msg <- "`row_spec()` does not support these arguments for `tabularray` tables: `font_size`, `angle`, `align`. Please use the `cell_spec()` function."
        stop(msg, call. = FALSE)
    }
    if (any(sapply(list(bold, italic, monospace, underline, strikeout), length) != 1)) {
        msg <- "`row_spec()` requires these arguments to be of length 1 for `tabularray` tables: `bold`, `italic`, `underline`, `strikeout`. Please use the `cell_spec()` function."
        stop(msg, call. = FALSE)
    }
    if (any(sapply(list(background, color), function(x) !is.null(x) && length(x) != 1))) {
        msg <- "`row_spec()` requires these arguments to be `NULL` or of length 1 for `tabularray` tables: `background`, `color`. Please use the `cell_spec()` function."
        stop(msg, call. = FALSE)
    }

    # before row settings
    out <- define_colors_tabularray(out,background = background, color = color)
    if (is.character(color)) color <- sub("^#", "c", color)
    if (is.character(background)) background <- sub("^#", "c", background)

    row <- table_info$position_offset + row

    for (r in row) {
        if (is.character(color)) rowspec[[r]][["fg"]] <- color
        if (is.character(background)) rowspec[[r]][["bg"]] <- background

        font <- ""
        if (bold) font <- paste0(font, "\\\\bfseries")
        if (monospace) font <- paste0(font, "\\\\ttfamily")
        if (italic) font <- paste0(font, "\\\\itshape")

        cmd <- ""
        if (underline) cmd <- paste0(cmd, "\\\\kableExtraTabularrayUnderline{#1}")
        if (strikeout) cmd <- paste0(cmd, "\\\\kableExtraTabularrayStrikeout{#1}")

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

    # priority rowspec over colspec
    out <- tmp <- strsplit(out, "\\n")[[1]]
    idx_col <- grep("^colspec=", tmp)[1]
    idx_row <- grep("^rowspec=", tmp)[1]
    out[min(c(idx_col, idx_row))] <- tmp[idx_col]
    out[max(c(idx_col, idx_row))] <- tmp[idx_row]
    out <- paste(out, collapse = "\n")

    table_info$tabularray$rowspec <- rowspec
    out <- structure(out, format = "latex", class = "knitr_kable")
    attr(out, "kable_meta") <- table_info

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
    table_info <- magic_mirror(out)
    vline <- table_info$tabularray$vline
    colspec <- table_info$tabularray$colspec

    # assertions
    # the reason why we force arguments to be scalar is that that vector
    # arguments are fundamentally at odds with the design philosophy of
    # tabularray, which separates style and data, and applies styles to complete
    # rows and columns. it makes more sense to return an informative pointing
    # users to cell_spec(), which can easily deal with the same use case.
    if (!is.null(latex_column_spec)) {
        msg <- "latex_column_spec not supported with tabularray"
        stop(msg, call. = FALSE)
    }
    if (any(!column %in% seq_along(colspec))) {
        msg <- sprintf("`column_spec()` error: `column` must be a vector of unique integers between 1 and %s.", length(colspec))
        stop(msg, call. = FALSE)
    }
    if (any(sapply(list(bold, italic, monospace, underline, strikeout, latex_valign), length) != 1)) {
        msg <- "`column_spec()` requires these arguments to be of length 1 for `tabularray` tables: `bold`, `italic`, `underline`, `strikeout`, `latex_valign`. Please use the `cell_spec()` function."
        stop(msg, call. = FALSE)
    }
    if (any(sapply(list(background, color), function(x) !is.null(x) && length(x) != 1))) {
        msg <- "`column_spec()` requires these arguments to be `NULL` or of length 1 for `tabularray` tables: `background`, `color`. Please use the `cell_spec()` function."
        stop(msg, call. = FALSE)
    }

    # tabularray calls it m instead of p
    valign <- if (latex_valign == "p") latex_valign <- "m"

    # before column settings
    out <- define_colors_tabularray(out,background = background, color = color)
    if (is.character(color)) color <- sub("^#", "c", color)
    if (is.character(background)) background <- sub("^#", "c", background)

    for (col in column) {
        if (is.character(width)) colspec[[col]][["wd"]] <- width
        if (is.character(color)) colspec[[col]][["fg"]] <- color
        if (is.character(background)) colspec[[col]][["bg"]] <- background
        if (is.character(valign)) colspec[[col]][["valign"]] <- valign

        font <- ""
        if (bold) font <- paste0(font, "\\\\bfseries")
        if (monospace) font <- paste0(font, "\\\\ttfamily")
        if (italic) font <- paste0(font, "\\\\itshape")

        cmd <- ""
        if (underline) cmd <- paste0(cmd, "\\\\kableExtraTabularrayUnderline{#1}")
        if (strikeout) cmd <- paste0(cmd, "\\\\kableExtraTabularrayStrikeout{#1}")

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

    # priority colspec over rowspec
    out <- tmp <- strsplit(out, "\\n")[[1]]
    idx_col <- grep("^colspec=", tmp)[1]
    idx_row <- grep("^rowspec=", tmp)[1]
    out[min(c(idx_col, idx_row))] <- tmp[idx_row]
    out[max(c(idx_col, idx_row))] <- tmp[idx_col]
    out <- paste(out, collapse = "\n")

    table_info$tabularray$colspec <- colspec
    out <- structure(out, format = "latex", class = "knitr_kable")
    attr(out, "kable_meta") <- table_info

    return(out)
}


cell_spec_tabularray <- function(
    x,
    bold = FALSE,
    italic = FALSE,
    monospace = FALSE,
    underline = FALSE,
    strikeout = FALSE,
    color = "black",
    background = "white",
    align = "c",
    font_size = NULL,
    angle = NULL,
    escape = TRUE,
    multicolumn = 1,
    multirow = 1,
    ...) {

    if (escape) x <- escape_latex(x)

    background <- ifelse(is.null(background), "", paste0("bg=", background))
    color <- ifelse(is.null(color), "", paste0("fg=", color))

    font <- rep("", length(x))
    font <- ifelse(bold, paste0(font, "\\\\bfseries"), font)
    font <- ifelse(monospace, paste0(font, "\\\\ttfamily"), font)
    font <- ifelse(italic, paste0(font, "\\\\itshape"), font)
    font <- ifelse(font != "", paste0("font=", font), font)

    cmd <- rep("", length(x))
    cmd <- ifelse(underline, paste0(cmd, "\\\\kableExtraTabularrayUnderline{\\#1}"), cmd)
    cmd <- ifelse(strikeout, paste0(cmd, "\\\\kableExtraTabularrayStrikeout{\\#1}"), cmd)
    cmd <- ifelse(cmd != "", paste0("cmd=", cmd), cmd)

    if (!is.null(font_size)) {
        x <- paste0(
            "\\bgroup\\fontsize{", font_size, "}{", as.numeric(font_size) + 2,
            "}\\selectfont ", x, "\\egroup{}")
    }
    if (!is.null(angle)) x <- paste0("\\rotatebox{", angle, "}{", x, "}")
    if (is.null(align)) {
        halign <- "halign=c"
    } else {
        halign <- paste0("halign=", align)
    }

    placeholder <- c(background, color, font, cmd, halign)
    placeholder <- Filter(function(x) x != "", placeholder)
    placeholder <- paste(placeholder, collapse = ", ")
    x <- sprintf(
        "\\SetCell[r=%s, c=%s]{%s}%s",
        multirow,
        multicolumn,
        placeholder,
        x)
    x <- sub("SetCell[r=1, c=1]", "SetCell", x, fixed = TRUE)
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


init_tabularray <- function(
    kable_input, format, digits, row.names, col.names, align, caption, label, format.args,
    escape, table.attr, booktabs, longtable, tabular, valign, position,
    centering, vline, toprule, bottomrule, midrule, linesep, caption.short,
    table.envir) {

    table_info <- magic_mirror(kable_input)
    out <- kable_input

    if (is.null(vline)) vline <- ""

    # extract align from \begin{tblr}[t]{lrcc}
    tab_split <- strsplit(out, "\\n")[[1]]
    idx <- grep("begin\\{tblr\\}|begin\\{talltblr\\}|begin\\{longtblr\\}", tab_split)
    align <- tab_split[idx]
    align <- gsub(".*\\{(.*)}", "\\1", align)
    align <- strsplit(align, split = "|", fixed = TRUE)[[1]]
    align <- lapply(align, function(x) list(type = "Q", halign = x))
    align <- sapply(align, make_spec_tabularray)

    # caption
    if (is.character(caption)) {
        caption <- paste0("caption={", caption, "}")
    } else {
        caption <- ""
    }

    # basic rows
    rowspec <- rep("Q[]", table_info$nrow + table_info$position_offset)
    rowspec_string <- paste(rowspec, collapse = "")

    tmp <- sprintf(
        "\\begin{%s}[         %% tabularray outer open
%s,
]                     %% tabularray outer close
{                     %% tabularray inner open
colspec={%s},
rowspec={%s},
}                     %% tabularray inner close",
        table_info$tabular,
        caption,
        paste(align, collapse = vline),
        rowspec_string
    )

    # cleanup table
    tmp <- strsplit(tmp, "\\n")[[1]]
    tab_split[idx] <- paste(tmp[!tmp %in% c("", ",")], collapse = "\n")

    # prepare new table
    out <- paste(tab_split, collapse = "\n")

    table_info$tabularray <- list(
        colspec = lapply(align, parse_spec_tabularray),
        rowspec = lapply(rowspec, parse_spec_tabularray),
        vline = vline,
        linesep = linesep
    )

    out <- structure(out, format = "latex", class = "knitr_kable")
    attr(out, "kable_meta") <- table_info

    return(out)
}


styling_tabularray <- function(x, tabularray_inner, tabularray_outer) {
    if (is.null(tabularray_inner) && is.null(tabularray_outer)) return(x)

    tab <- strsplit(x, "\\n")[[1]]

    # outer
    if (!is.null(tabularray_outer)) {
        idx <- grep("% tabularray outer open", tab, fixed = TRUE)[1]
        out <- c(
            tab[seq_len(idx)],
            paste0(paste(tabularray_outer, collapse = ", "), ","),
            tab[(idx + 1):length(tab)])
        tab <- out
    }

    # inner
    if (!is.null(tabularray_inner)) {
        idx <- grep("tabularray inner open", tab, fixed = TRUE)[1]
        out <- c(
            tab[seq_len(idx)],
            paste0(paste(tabularray_inner, collapse = ", "), ","),
            tab[(idx + 1):length(tab)])
        tab <- out
    }

    out <- paste(out, collapse = "\n")
    return(out)
}

styling_latex_full_width_tabularray <- function(x, table_info){
    colspec <- table_info$tabularray$colspec
    out <- strsplit(x, "\\n")[[1]]
    idx <- grep("^colspec=", out)
    if (!is.na(idx)) {
        out[idx[1]] <- gsub("Q[", "X[", out[idx[1]], fixed = TRUE)
    }
    for (i in seq_along(table_info$tabularray$colspec)) {
      table_info$tabularray$colspec[[i]][["type"]] <- "X"
    }
    out <- paste(out, collapse = "\n")
    out <- structure(out, format = "latex", class = "knitr_kable")
    attr(out, "kable_meta") <- table_info
    return(list(out, NULL))
}


add_header_above_tabularray <- function(
    kable_input,
    header = NULL,
    bold = FALSE,
    italic = FALSE,
    monospace = FALSE,
    underline = FALSE,
    strikeout = FALSE,
    align = "c",
    color = NULL,
    background = NULL,
    font_size = NULL,
    angle = NULL,
    escape = TRUE,
    line = TRUE,
    line_sep = 3,
    include_empty = FALSE,
    border_left = FALSE,
    border_right = FALSE,
    ...) {

    if (is.null(header)) {
        return(kable_input)
    }

    out <- kable_input
    table_info <- magic_mirror(kable_input)

    # sanity checks
    if (length(table_info$tabularray$colspec) != sum(header$colspan)) {
        stop("The length of `header` must be equal to the number of columns in the table.", call. = FALSE)
    }

    cells <- sapply(seq_len(nrow(header)), function(i) {
        cell_spec_tabularray(
            header$header[i],
            multicolumn = header$colspan[i],
            multirow = 1,
            bold = bold,
            italic = italic,
            monospace = monospace,
            underline = underline,
            strikeout = strikeout,
            align = align,
            color = color,
            background = background,
            font_size = font_size,
            angle = angle,
            escape = escape)
    })

    idx <- seq_len(length(cells) - 1)
    new_row <- cells
    new_row[idx] <- paste(new_row, strrep("&", header$colspan))[idx]
    new_row <- paste(new_row, collapse = " ")
    new_row <- paste(new_row, "\\\\")

    if (line) {
        midrule <- sprintf("\\cmidrule[lr]{%s-%s}", header$start, header$end)
        midrule <- midrule[trimws(header$header) != ""]
        midrule <- paste(midrule, collapse = " ")
        new_row <- c(new_row, midrule)
    }

    # Insert at the top of the table, below \toprule
    out <- strsplit(out, "\\n")[[1]]
    idx <- grep("tabularray inner close", out)
    flag <- grepl("\\\\toprule|\\\\hline|\\\\midrule", out[idx + 1])
    if (flag) {
        idx <- idx + 1
    }
    out <- c(
        out[seq_len(idx)],
        new_row,
        out[(idx + 1):length(out)])
    out <- paste(out, collapse = "\n")

    # New row in colspec
    new_row <- "rowspec={Q[]"
    out <- sub("rowspec={", new_row, out, fixed = TRUE)

    out <- structure(out, format = "latex", class = "knitr_kable")
    attr(out, "kable_meta") <- table_info

    return(out)
}


define_colors_tabularray <- function(x, background = NULL, color = NULL) {
    colors <- unique(c(background, color))
    colors <- Filter(function(x) grepl("^#\\w{6}", x), colors)
    if (length(colors) > 0) {
        colors <- sprintf(
            "\\kableExtraDefineColor{%s}{HTML}{%s}",
            sub("^#", "c", colors),
            sub("^#", "", colors))
        colors <- paste(colors, collapse = "")
        x <- strsplit(x, "\\n")[[1]]
        idx <- grep("% tabularray inner close", x, fixed = TRUE)[1]
        x <- c(x[1:idx], colors, x[(idx + 1):length(x)])
        x <- paste(x, collapse = "\n")
    }
    return(x)
}
