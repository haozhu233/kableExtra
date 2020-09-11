#' Helper functions to generate inline sparklines
#'
#' @description These functions helps you quickly generate sets of sparkline
#' style plots using base R plotting system. Currently, we support histogram,
#' boxplot, and line. You can use them together with `column_spec` to
#' generate inline plot in tables. By default, this function will save images
#' in a folder called "kableExtra" and return the address of the file.
#'
#' The `spec_plot` function is special in that it takes a
#' user-generated *function* instead of lists of data. It handles both
#' base graphics and `ggplot2` graphics similarly. It is up to the
#' user to control margins and themes so that the plot will not error
#' and that it is reasonably formatted for the page. For base
#' graphics, one example of controlling this would be to start with:
#'
#' ```
#' plot(..., xaxt = "n", yaxt = "n", ann = FALSE, frame.plot = FALSE)
#' ```
#'
#' For `ggplot2` graphics, the following theme (`theme_minimal` plus a
#' little extra) mimics the above:
#'
#' ```r
#' theme(
#'   line = element_blank(),
#'   text = element_blank(),
#'   axis.ticks = element_blank(),
#'   axis.ticks.length = unit(0, "mm"),
#'   axis.title = element_blank(),
#'   axis.text = element_blank(),
#'   axis.line = element_blank(),
#'   title = element_blank(),
#'   panel.grid = element_blank(),
#'   panel.border = element_blank(),
#'   panel.background = element_blank(),
#'   plot.background = element_blank(),
#'   strip.background = element_blank(),
#'   plot.margin = unit(rep(0, 4), "mm")
#' )
#' ```
#'
#' @param x Vector of values or List of vectors of values.
#' @param width The width of the plot in pixel
#' @param height The height of the plot in pixel
#' @param res The resolution of the plot. Default is 300.
#' @param same_lim T/F. If x is a list of vectors, should all the plots be
#' plotted in the same range? Default is True.
#' @param lim Manually specify plotting range in the form of
#' `c(0, 10)`.
#' @param xaxt On/Off for xaxis text
#' @param yaxt On/Off for yaxis text
#' @param ann On/Off for annotations (titles and axis titles)
#' @param col Color for the fill of the histogram bar/boxplot box.
#' @param border Color for the border.
#' @param dir Directory of where the images will be saved.
#' @param file File name. If not provided, a random name will be used
#' @param file_type Graphic device. Support `png` or `svg`. SVG is recommended
#' for HTML output
#' @param ... extra parameters sending to `hist()`
#'
#' @export
spec_hist <- function(x, width = 200, height = 50, res = 300,
                      breaks = "Sturges",
                      same_lim = TRUE, lim = NULL,
                      xaxt = 'n', yaxt = 'n', ann = FALSE,
                      col = "lightgray", border = NULL,
                      dir = if (is_latex()) rmd_files_dir() else tempdir(),
                      file = NULL,
                      file_type = if (is_latex()) "png" else "svg", ...) {
  if (is.list(x)) {
    if (same_lim & is.null(lim)) {
      lim <- base::range(unlist(x))
    }
    return(lapply(x, function(x_) {spec_hist(
      x = x_, width = width, height = height,
      breaks = breaks, same_lim = same_lim, lim = lim,
      xaxt = xaxt, yaxt = yaxt, ann = ann, col = col, border = border,
      dir = dir, file = file, file_type = file_type, ...
    )}))
  }

  if (is.null(lim)) {
    lim <- base::range(x)
  }

  file_type <- match.arg(file_type, c("svg", "png"))

  if (!dir.exists(dir)) {
    dir.create(dir)
  }

  if (is.null(file)) {
    file <- file.path(dir, paste0(
      "hist_", round(as.numeric(Sys.time()) * 1000), ".", file_type))
  }

  if (file_type == "svg") {
    grDevices::svg(filename = file, width = width / res, height = height / res,
                   bg = 'transparent')
  } else {
    grDevices::png(filename = file, width = width, height = height, res = res,
                   bg = 'transparent')
  }

  graphics::par(mar = c(0, 0, 0.2, 0), lwd=0.5)
  graphics::hist(x, breaks = breaks, xlim = lim, border = border,
                 xaxt = xaxt, yaxt = yaxt, ann = ann, col = col, ...)
  grDevices::dev.off()

  if (file_type == "svg") {
    svg_xml <- xml2::read_xml(file)
    svg_text <- as.character(svg_xml)
    unlink(file)
  } else {
    svg_text <- NULL
  }
  out <- list(path = file, dev = file_type, type = "hist",
              width = width, height = height, res = res,
              svg_text = svg_text)

  class(out) <- "kableExtraInlinePlots"
  return(out)
}

#' Helper functions to generate inline sparklines
#'
#' @description These functions helps you quickly generate sets of sparkline
#' style plots using base R plotting system. Currently, we support histogram,
#' boxplot, and line. You can use them together with `column_spec` to
#' generate inline plot in tables. By default, this function will save images
#' in a folder called "kableExtra" and return the address of the file.
#'
#' @param x Vector of values or List of vectors of values.
#' @param width The width of the plot in pixel
#' @param height The height of the plot in pixel
#' @param res The resolution of the plot. Default is 300.
#' @param add_label For boxplot. T/F to add labels for min, mean and max.
#' @param label_digits If T for add_label, rounding digits for the label.
#' Default is 2.
#' @param same_lim T/F. If x is a list of vectors, should all the plots be
#' plotted in the same range? Default is True.
#' @param lim,xlim,ylim Manually specify plotting range in the form of
#' `c(0, 10)`. `lim` is used in `spec_hist` and `spec_boxplot`; `xlim`
#' and `ylim` are used in `spec_line`.
#' @param xaxt On/Off for xaxis text
#' @param yaxt On/Off for yaxis text
#' @param ann On/Off for annotations (titles and axis titles)
#' @param col Color for the fill of the histogram bar/boxplot box.
#' @param border Color for the border.
#' @param boxlty Boxplot - box boarder type
#' @param medcol Boxplot - median line color
#' @param medlwd Boxplot - median line width
#' @param dir Directory of where the images will be saved.
#' @param file File name. If not provided, a random name will be used
#' @param file_type Graphic device. Support `png` or `svg`. SVG is recommended
#' for HTML output
#' @param ... extraparameters passing to boxplot
#'
#' @export
spec_boxplot <- function(x, width = 200, height = 50, res = 300,
                         add_label = FALSE, label_digits = 2,
                         same_lim = TRUE, lim = NULL,
                         xaxt = 'n', yaxt = 'n', ann = FALSE,
                         col = "lightgray", border = NULL,
                         boxlty = 0, medcol = "red", medlwd = 1,
                         dir = if (is_latex()) rmd_files_dir() else tempdir(),
                         file = NULL,
                         file_type = if (is_latex()) "png" else "svg", ...) {
  if (is.list(x)) {
    if (same_lim & is.null(lim)) {
      lim <- base::range(unlist(x))
    }
    return(lapply(x, function(x_) {spec_boxplot(
      x = x_, width = width, height = height,
      add_label = add_label, same_lim = same_lim, lim = lim,
      xaxt = xaxt, yaxt = yaxt, ann = ann,
      col = col, border = border,
      boxlty = boxlty, medcol = medcol, medlwd = medlwd,
      dir = dir, file = file, file_type = file_type, ...
    )}))
  }

  if (is.null(lim)) {
    lim <- base::range(x)
    lim[1] <- lim[1] - (lim[2] - lim[1]) / 10
    lim[2] <- (lim[2] - lim[1]) / 10 + lim[2]
  }

  file_type <- match.arg(file_type, c("svg", "png"))

  if (!dir.exists(dir)) {
    dir.create(dir)
  }

  if (is.null(file)) {
    file <- file.path(dir, paste0(
      "hist_", round(as.numeric(Sys.time()) * 1000), ".", file_type))
  }

  if (file_type == "svg") {
    grDevices::svg(filename = file, width = width / res, height = height / res,
                   bg = 'transparent')
  } else {
    grDevices::png(filename = file, width = width, height = height, res = res,
                   bg = 'transparent')
  }

  graphics::par(mar = c(0, 0, 0, 0))

  graphics::boxplot(x, horizontal = TRUE, ann = ann, frame = FALSE, bty = 'n', ylim = lim,
          col = col, border = border,
          boxlty = boxlty, medcol = medcol, medlwd = medlwd,
          axes = FALSE, outcex = 0.2, whisklty = 1,
          ...)
  if (add_label) {
    x_median <- round(median(x, na.rm = T), label_digits)
    x_min <- round(min(x, na.rm = T), label_digits)
    x_max <- round(max(x, na.rm = T), label_digits)
    graphics::text(x_median, y = 1.4, labels = x_median, cex = 0.5)
    graphics::text(x_min, y = 0.6, labels = x_min, cex = 0.5)
    graphics::text(x_max, y = 0.6, labels = x_max, cex = 0.5)
  }
  grDevices::dev.off()

  if (file_type == "svg") {
    svg_xml <- xml2::read_xml(file)
    svg_text <- as.character(svg_xml)
    unlink(file)
  } else {
    svg_text <- NULL
  }
  out <- list(path = file, dev = file_type, type = "boxplot",
              width = width, height = height, res = res,
              svg_text = svg_text)
  class(out) <- "kableExtraInlinePlots"
  return(out)
}

is_latex <- knitr::is_latex_output

rmd_files_dir <- function(create = TRUE) {
  curr_file_name <- sub("\\.[^\\.]*$", "", knitr::current_input())
  dir_name <- paste0(curr_file_name, "_files")
  if (!dir.exists(dir_name) & create) dir.create(dir_name)
  fig_dir_name <- file.path(dir_name, "figure-latex/")
  if (!dir.exists(fig_dir_name) & create) dir.create(fig_dir_name)
  return(fig_dir_name)
}

#' Helper functions to generate inline sparklines
#'
#' @description These functions helps you quickly generate sets of sparkline
#' style plots using base R plotting system. Currently, we support histogram,
#' boxplot, and line. You can use them together with `column_spec` to
#' generate inline plot in tables. By default, this function will save images
#' in a folder called "kableExtra" and return the address of the file.
#'
#' @param x,y Vector of values or List of vectors of values. y is optional.
#' @param width The width of the plot in pixel
#' @param height The height of the plot in pixel
#' @param res The resolution of the plot. Default is 300.
#' @param same_lim T/F. If x is a list of vectors, should all the plots be
#' plotted in the same range? Default is True.
#' @param xlim,ylim Manually specify plotting range in the form of
#' `c(0, 10)`.
#' @param xaxt On/Off for xaxis text
#' @param yaxt On/Off for yaxis text
#' @param ann On/Off for annotations (titles and axis titles)
#' @param col Color for the fill of the histogram bar/boxplot box.
#' @param border Color for the border.
#' @param frame.plot On/Off for surrounding box (`spec_line` only). Default
#' is False.
#' @param lwd Line width for `spec_line`; within `spec_line`, the `minmax`
#' argument defaults to use this value for `cex` for points. Default is 2.
#' @param minmax,min,max Arguments passed to `points` to highlight minimum
#' and maximum values in `spec_line`. If `min` or `max` are `NULL`, they
#' default to the value of `minmax`. Set to an empty `list()` to disable.
#' @param dir Directory of where the images will be saved.
#' @param file File name. If not provided, a random name will be used
#' @param file_type Graphic device. Support `png` or `svg`. SVG is recommended
#' for HTML output.
#' @param ... extra parameters passing to `plot`
#'
#' @export
spec_line <- function(x, y = NULL, width = 200, height = 50, res = 300,
                      same_lim = TRUE, xlim = NULL, ylim = NULL,
                      xaxt = 'n', yaxt = 'n', ann = FALSE,
                      col = "lightgray", border = NULL,
                      frame.plot = FALSE, lwd = 2,
                      minmax = list(pch = ".", cex = lwd, col = "red"),
                      min = minmax, max = minmax,
                      dir = if (is_latex()) rmd_files_dir() else tempdir(),
                      file = NULL,
                      file_type = if (is_latex()) "png" else "svg", ...) {
  if (is.list(x)) {
    if (same_lim) {
      if (is.null(xlim)) {
        xlim <- base::range(unlist(x))
      }
      if (is.null(ylim) && !is.null(y)) {
        ylim <- base::range(unlist(y))
      }
    }
    if (is.null(y)) {
      y <- replicate(length(x), NULL, simplify = FALSE)
    } else if (!is.list(y) || length(x) != length(y)) {
      stop("'x' and 'y' are not the same length")
    }
    return(Map(function(x_, y_) {
      spec_line(x = x_, y = y_,
                width = width, height = height,
                same_lim = same_lim, xlim = xlim, ylim = ylim,
                xaxt = xaxt, yaxt = yaxt, ann = ann, col = col, border = border,
                frame.plot = frame.plot, lwd = lwd,
                minmax = minmax, min = min, max = max,
                dir = dir, file = file, file_type = file_type, ...)
    }, x, y))
  }

  if (is.null(y) || !length(y)) {
    y <- x
    x <- seq(0, 1, length.out = length(y))
    tmp <- ylim
    ylim <- xlim
    xlim <- tmp
  }

  if (is.null(xlim)) {
    xlim <- base::range(x)
  }

  if (is.null(ylim) && !is.null(y)) {
    ylim <- base::range(y)
  }

  if (is.null(min)) min <- minmax
  if (is.null(max)) max <- minmax

  expand <- c(
    if (!is.null(min) && length(min)) -0.04 else 0,
    if (!is.null(max) && length(max)) +0.04 else 0)
  xlim <- xlim + diff(xlim) * expand
  ylim <- ylim + diff(ylim) * expand

  file_type <- match.arg(file_type, c("svg", "png"))

  if (!dir.exists(dir)) {
    dir.create(dir)
  }

  if (is.null(file)) {
    file <- file.path(dir, paste0(
      "hist_", round(as.numeric(Sys.time()) * 1000), ".", file_type))
  }

  if (file_type == "svg") {
    grDevices::svg(filename = file, width = width / res, height = height / res,
                   bg = 'transparent')
  } else {
    grDevices::png(filename = file, width = width, height = height, res = res,
                   bg = 'transparent')
  }
  curdev <- grDevices::dev.cur()
  on.exit(grDevices::dev.off(curdev), add = TRUE)

  graphics::par(mar = c(0, 0, 0.2, 0), lwd = lwd)
  graphics::plot(x, y, type = "l", xlim = xlim, ylim = ylim,
                 xaxt = xaxt, yaxt = yaxt, ann = ann, col = col,
                 frame.plot = frame.plot, ...)

  if (!is.null(min) && length(min)) {
    ind <- which.min(y)
    do.call(graphics::points, c(list(x[ind], y[ind], xpd = NA), min))
  }

  if (!is.null(max) && length(max)) {
    ind <- which.max(y)
    do.call(graphics::points, c(list(x[ind], y[ind], xpd = NA), max))
  }

  grDevices::dev.off(curdev)

  if (file_type == "svg") {
    svg_xml <- xml2::read_xml(file)
    svg_text <- as.character(svg_xml)
    unlink(file)
  } else {
    svg_text <- NULL
  }
  out <- list(path = file, dev = file_type, type = "line",
              width = width, height = height, res = res,
              svg_text = svg_text)

  class(out) <- "kableExtraInlinePlots"
  return(out)
}

#' @rdname spec_hist
#' @export
spec_plot <- function(fun, ..., width = 200, height = 50, res = 300,
                      dir = if (is_latex()) rmd_files_dir() else tempdir(),
                      file = NULL, file_type = if (is_latex()) "png" else "svg",
                      islist = TRUE) {

  dots <- list(...)
  if (islist) {
    out <- do.call(Map, c(list(f = spec_plot, fun = list(fun), islist = list(FALSE),
                               width = list(width), height = list(height),
                               res = list(res), dir = list(dir), file = list(file),
                               file_type = list(file_type)), dots))
    return(out)
  }

  file_type <- match.arg(file_type, c("svg", "png"))

  if (!dir.exists(dir)) {
    dir.create(dir)
  }

  if (is.null(file)) {
    file <- file.path(dir, paste0(
      "hist_", round(as.numeric(Sys.time()) * 1000), ".", file_type))
  }

  if (file_type == "svg") {
    grDevices::svg(filename = file, width = width / res, height = height / res,
                   bg = 'transparent')
  } else {
    grDevices::png(filename = file, width = width, height = height, res = res,
                   bg = 'transparent')
  }
  curdev <- grDevices::dev.cur()
  on.exit(grDevices::dev.off(curdev), add = TRUE)

  thisplot <- do.call(fun, dots)

  grDevices::dev.off(curdev)

  if (inherits(thisplot, "ggplot")) {
    # assume that ggplot2 is loaded/available, we'll overwrite the
    # previous file created since it likely didn't save correctly
    ggsave <- tryCatch(get("ggsave", envir=as.environment("package:ggplot2")),
                       error = function(e) e)
    if (inherits(ggsave, "error") || !is.function(ggsave)) {
      stop("unable to find 'ggplot2::ggsave': ", conditionMessage(e))
    }
    if (file_type == "svg") {
      ggsave(file = file, plot = thisplot, device = grDevices::svg,
             width = width / res, height = height / res)
    } else {
      ggsave(file = file, plot = thisplot,
             width = width / res, height = height / res)
    }
  }

  if (file_type == "svg") {
    svg_xml <- xml2::read_xml(file)
    svg_text <- as.character(svg_xml)
    unlink(file)
  } else {
    svg_text <- NULL
  }
  out <- list(path = file, dev = file_type, type = "line",
              width = width, height = height, res = res,
              svg_text = svg_text)

  class(out) <- "kableExtraInlinePlots"
  return(out)

}
