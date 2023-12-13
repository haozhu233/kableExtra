
# TODO:
# - [ ] booktabs not supported
# - [ ] longtable -> longtblr
# - [ ] `border_right` and `border_left` are profoundly broken
# - [ ] `vline=""` breaks replacement of Q[] because sub is looking for "l|r|r"


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

    # TODO: 
    # - [ ] font_size,
    # - [ ] angle,
    # - [ ] hline_after,

    out <- kable_input

    table_info <- magic_mirror(kable_input)

    vectorize_style <- function(s, row) {
        if (is.null(s)) {
            return(rep("", length(row)))
        }

        if (length(s) == 1) {
            out <- rep(s, length(row))
        } else {
            out <- s
        }
        if (length(out) != length(row)) {
            msg <- sprintf("`row_spec()` error: length of style vector must be the same as the length of the `row` index vector.", length(row))
            stop(msg, .call = FALSE)
        }
        return(out)
    }

    bold <- vectorize_style(bold, row)
    italic <- vectorize_style(italic, row)
    monospace <- vectorize_style(monospace, row)
    underline <- vectorize_style(underline, row)
    strikeout <- vectorize_style(strikeout, row)
    color <- vectorize_style(color, row)
    background <- vectorize_style(background, row)
    align <- vectorize_style(align, row)

    row <- row + table_info$position_offset

    for (r in seq_along(row)) {
        i <- row[r]
        target_row <- table_info$contents[i]

        # SetRow override
        new_row <- sub("\\\\SetRow\\{[^\\}]*\\}", "", target_row)

        new_row <- latex_new_row_builder(
            target_row = new_row,
            table_info = table_info,
            bold = FALSE,
            italic = FALSE,
            monospace = FALSE,
            underline = underline[r],
            strikeout = strikeout[r],
            color = NULL,
            background = NULL,
            align = NULL,
            font_size = NULL,
            angle = NULL,
            hline_after = FALSE,
            extra_latex_after = extra_latex_after)


        # SetRow settings
        font <- rep("", length(row))
        font <- ifelse(bold, paste0(font, "\\\\bfseries"), font)
        font <- ifelse(monospace, paste0(font, "\\\\ttfamily"), font)
        font <- ifelse(italic, paste0(font, "\\\\itshape"), font)

        new_row <- sprintf(
            "\\\\SetRow{%s, bg=%s, fg=%s, font=%s} %s",
            align[r],
            background[r],
            color[r],
            font[r],
            new_row
        )

        tmp <- latex_new_row_replacer(
            i = i,
            row = row,
            out = out,
            table_info = table_info,
            new_row = new_row,
            target_row = target_row)
        out <- tmp$out
        table_info <- tmp$table_info
    }

    out <- structure(out, format = "latex", class = "knitr_kable")
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

    if (any(!column %in% seq_along(colspec))) {
        msg <- sprintf("`column_spec()` error: column index must be between 1 and %s.", length(colspec))
        stop(msg, call. = FALSE)
    }

    for (col in column) {
        colspec[[col]][["wd"]] <- ifelse(is.character(width), width, "")
        colspec[[col]][["fg"]] <- ifelse(is.character(color), color, "")
        colspec[[col]][["bg"]] <- ifelse(is.character(background), background, "")

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

    colspec_vector <- unlist(lapply(colspec, make_column_tabularray))
    colspec_string <- sprintf("colspec={%s},", paste(colspec_vector, collapse = vline))

    # assumes that colspec is on its own line
    out <- sub("colspec=.*", colspec_string, out, perl = TRUE)

    attr(out, "tabularray_colspec") <- colspec

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


parse_column_tabularray <- function(column) {
  # Extracting the arguments within square brackets
  args_str <- gsub("[Q|X]\\[(.*)\\].*", "\\1", column)
  # Splitting the arguments by comma
  args <- trimws(strsplit(args_str, ",")[[1]])
  # Argument list
  keys <- trimws(gsub("=.*", "", args))
  values <- trimws(gsub(".*=", "", args))
  args <- as.list(setNames(values, keys))
  args$coltype <- ifelse(grepl("^X", column), "X", "Q")
  return(args)
}


make_column_tabularray <- function(args_list) {
    coltype <- args_list$coltype
    args_list$coltype <- NULL
    args_list <- paste(names(args_list), args_list, sep = "=")
    args_list <- paste(args_list, collapse = ", ")
    out <- sprintf("%s[%s]", coltype, args_list)
    return(out)
}


preprocess_tabularray <- function(x) {
    cl <- attr(x, "call")
    table_info <- magic_mirror(x)

    if (isTRUE(cl$bootabs) || isTRUE(table_info$booktabs)) {
        stop("booktabs not supported with tabularray", call. = FALSE)
    }

    if (!is.null(cl$vline)) {
        vline <- cl$vline
    } else {
        vline <- "|"
    }

    # extract align from \begin{tblr}[t]{lrcc}
    tab_split <- strsplit(x, "\\n")[[1]]
    idx <- grep("begin\\{tblr\\}", tab_split)
    align <- tab_split[idx]
    align <- gsub(".*\\{(.*)}", "\\1", align)
    align <- strsplit(align, split = vline, fixed = TRUE)[[1]]
    align <- lapply(align, function(x) list(coltype = "Q", halign = x))
    align <- sapply(align, make_column_tabularray)

    # insert new align
    tab_split[idx] <- sprintf(
        "\\begin{tblr}{
colspec={%s},
}",
        paste(align, collapse = vline)
    )

    # prepare new table
    out <- paste(tab_split, collapse = "\n")
    class(out) <- class(x)
    for (n in names(attributes(x))) {
        attr(out, n) <- attr(x, n)
    }
    attr(out, "tabularray_colspec") <- lapply(align, parse_column_tabularray)
    attr(out, "tabularray_vline") <- vline

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