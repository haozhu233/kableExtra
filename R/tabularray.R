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

    table_info <- magic_mirror(kable_input)
    rowspec <- table_info$tabularray$rowspec
    linesep <- table_info$tabularray$linesep
    out <- kable_input

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
        msg <- sprintf("`row_spec()` error: row index must be between 1 and %s.", length(rowspec))
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
    idx <- grep("begin\\{tblr\\}|begin\\{talltblr\\}|begin\\{longtblr\\}", tab_split)
    align <- tab_split[idx]
    align <- gsub(".*\\{(.*)}", "\\1", align)
    align <- strsplit(align, split = "|", fixed = TRUE)[[1]]
    align <- lapply(align, function(x) list(type = "Q", halign = x))
    align <- sapply(align, make_spec_tabularray)

    # caption
    if (is.character(cl[["caption"]])) {
        caption <- paste0("caption={", cl[["caption"]], "}")
    } else {
        caption <- ""
    }

    # basic rows
    rowspec <- rep("Q[]", table_info$nrow + table_info$position_offset)

    # let user specify their own linesep
    rowspec_string <- paste(rowspec, collapse = "")

    tmp <- sprintf(
        "\\begin{%s}[         %% tabularray square open
%s,
]                     %% tabularray square close
{                     %% tabularray curly open
colspec={%s},
rowspec={%s},
%s,
%s,
}                     %% tabularray curly close",
        table_info$tabular,
        caption,
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


styling_tabularray <- function(x, tabularray_options = NULL) {
    if (is.null(tabularray_options)) {
        return(x)
    }
    tab <- strsplit(x, "\\n")[[1]]
    idx <- which(tab %in% c("\\begin{tblr}[", "\\begin{talltblr}[", "\\begin{longtblr}["))[1]
    out <- c(
        tab[1:idx],
        paste0(paste(tabularray_options, collapse = ", "), ","),
        tab[(idx + 1):length(tab)])
    out <- paste(out, collapse = "\n")
    return(out)
}

styling_latex_full_width_tabularray <- function(x, table_info){
    colspec <- table_info$tabularray$colspec
    for (i in seq_along(colspec)) {
      colspec[[i]][["type"]] <- "X"
    }
    colspec_string <- lapply(colspec, make_spec_tabularray)
    colspec_string <- paste(colspec_string, collapse = table_info$tabularray$linesep)
    at <- attributes(x)
    at$tabularray$colspec <- colspec
    x <- sub("colspec=.*", paste0("colspec={", colspec_string, "},"), x, perl = TRUE)
    for (n in names(at)) {
        attr(x, n) <- at[[n]]
    }
    return(list(x, NULL))
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
    idx <- grep("tabularray curly close", out)
    flag <- grepl("\\\\toprule|\\\\hline|\\\\midrule", out[idx + 1])
    if (flag) {
        idx <- idx + 1
    }
    out <- c(
        out[1:idx],
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
