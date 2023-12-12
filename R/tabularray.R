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
    # TODO: hline_after is not supported
    # TODO: font_size, angle
    # TODO: vectorize bold et al.
    # TODO: DRY with row replacer

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
    # font_size <- vectorize_style(font_size, row)
    # angle <- vectorize_style(angle, row)

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
                                   font_size,
                                   angle,
                                   width,
                                   latex_valign,
                                   latex_column_spec) {
    # TODO: align_collapse vlines="|" (bug not specific to this PR)
    # TODO: support missing arguments
    table_info <- magic_mirror(kable_input)
    out <- kable_input

    if (!is.null(font_size)) stop("`font_size` is not supported by `column_spec()` for `tabularray` tables.", .call = FALSE)
    if (!is.null(angle)) stop("`angle` is not supported by `column_spec()` for `tabularray` tables.", .call = FALSE)
    if (!identical(latex_valign, "p")) stop("`latex_valign` is not supported by `column_spec()` for `tabularray` tables.", .call = FALSE)
    if (!is.null(latex_column_spec)) stop("`latex_column_spec` is not supported by `column_spec()` for `tabularray` tables.", .call = FALSE)

    # sanity check
    align <- table_info$align_vector
    if (length(align) != table_info$ncol || any(!table_info$align_vector %in% c("l", "r", "c"))) {
        msg <- 'When using `tabularray`, entries in the `align` argument must be "l", "r", or "c".'
        stop(msg, .call = FALSE)
    }

    vectorize_style <- function(s, column) {
        if (is.null(s)) {
            return(rep("", length(column)))
        }

        if (length(s) == 1) {
            out <- rep(s, length(column))
        } else {
            out <- s
        }
        if (length(out) != length(column)) {
            msg <- sprintf("`column_spec()` error: length of style vector must be the same as the length of the `column` index vector.", length(column))
            stop(msg, .call = FALSE)
        }
        return(out)
    }
    bold <- vectorize_style(bold, column)
    italic <- vectorize_style(italic, column)
    monospace <- vectorize_style(monospace, column)
    underline <- vectorize_style(underline, column)
    strikeout <- vectorize_style(strikeout, column)
    color <- vectorize_style(color, column)
    background <- vectorize_style(background, column)
    width <- vectorize_style(width, column)

    font <- rep("", length(row))
    font <- ifelse(bold, paste0(font, "\\\\bfseries"), font)
    font <- ifelse(monospace, paste0(font, "\\\\ttfamily"), font)
    font <- ifelse(italic, paste0(font, "\\\\itshape"), font)

    cmd <- rep("", length(row))
    cmd <- ifelse(underline, paste0(cmd, "\\\\kableExtraTabularrayUnderline{\\#1}"), cmd)
    cmd <- ifelse(strikeout, paste0(cmd, "\\\\kableExtraTabularrayStrikeout{\\#1}"), cmd)

    headers <- sprintf(
        "Q[wd=%s, align=%s, bg=%s, fg=%s, font=%s, cmd=%s]",
        width,
        align[column],
        background,
        color,
        font,
        cmd
    )

    align_collapse <- ifelse(table_info$booktabs | !is.null(table_info$xtable), "", "\\|")

    # old header settings
    align_collapse <- ifelse(table_info$booktabs | !is.null(table_info$xtable), "", "\\|")
    kable_align_old <- paste(table_info$align_vector, collapse = align_collapse)

    # new header settings
    table_info$align_vector[column] <- headers
    kable_align_new <- paste(table_info$align_vector, collapse = paste(align_collapse))

    out <- sub(paste0("\\{", kable_align_old, "\\}"),
        paste0("\\{", kable_align_new, "\\}"),
        out,
        perl = T)

    out <- structure(out, format = "latex", class = "knitr_kable")
    attr(out, "kable_meta") <- table_info
    return(out)
}
