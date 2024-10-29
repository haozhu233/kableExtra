#' Helper functions to generate inline sparklines
#'
#' @description These functions helps you quickly generate sets of sparkline
#' style plots using base R plotting system. Currently, we support histogram,
#' boxplot, line, scatter and pointrange plots. You can use them together with
#' `column_spec` to generate inline plot in tables. By default, this function
#' will save images in a folder called "kableExtra" and return the address of
#' the file.
#'
#' @param x Vector of values or List of vectors of values.
#' @param width The width of the plot in pixel
#' @param height The height of the plot in pixel
#' @param res The resolution of the plot. Default is 300.
#' @param breaks The `break` option in `hist`. Default is "Sturges" but you can
#' also provide a vector to manually specify break points.
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
#' @param file_type Graphic device. Can be character (e.g., `"pdf"`)
#'   or a graphics device function (`grDevices::pdf`). This defaults
#'   to `"pdf"` if the rendering is in LaTeX and `"svg"` otherwise.
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
                      file_type = if (is_latex()) "pdf" else svglite::svglite,
                      ...) {
  if (is.list(x)) {
    if (same_lim & is.null(lim)) {
      lim <- base::range(unlist(x), na.rm=TRUE)
    }

    dots <- listify_args(x, width, height, res, breaks,
                         lim, xaxt, yaxt, ann, col, border,
                         dir, file, file_type,
                         lengths = c(1, length(x)))
    return(do.call(Map, c(list(f = spec_hist), dots)))
  }

  if (is.null(x)) return(NULL)

  if (is.null(lim)) {
    lim <- base::range(x, na.rm=TRUE)
  }

  if (!dir.exists(dir)) {
    dir.create(dir)
  }

  file_ext <- dev_chr(file_type)
  if (is.null(file)) {
    file <- normalizePath(
      tempfile(pattern = "hist_", tmpdir = dir, fileext = paste0(".", file_ext)),
      winslash = "/", mustWork = FALSE)
  }

  graphics_dev(filename = file, dev = file_type,
               width = width, height = height, res = res,
               bg = "transparent")
  curdev <- grDevices::dev.cur()
  on.exit(grDevices::dev.off(curdev), add = TRUE)

  graphics::par(mar = c(0, 0, 0.2, 0), lwd=0.5)
  graphics::hist(x, breaks = breaks, xlim = lim, border = border,
                 xaxt = xaxt, yaxt = yaxt, ann = ann, col = col, ...)

  grDevices::dev.off(curdev)

  out <- make_inline_plot(
    file, file_ext, file_type,
    width, height, res,
    del = TRUE)
  return(out)
}

#' Helper functions to generate inline sparklines
#'
#' @description These functions helps you quickly generate sets of sparkline
#' style plots using base R plotting system. Currently, we support histogram,
#' boxplot, line, scatter and pointrange plots. You can use them together with
#' `column_spec` to generate inline plot in tables. By default, this function
#' will save images in a folder called "kableExtra" and return the address of
#' the file.
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
#' @param lim Manually specify plotting range in the form of
#' `c(0, 10)`.
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
#' @param file_type Graphic device. Can be character (e.g., `"pdf"`)
#'   or a graphics device function (`grDevices::pdf`). This defaults
#'   to `"pdf"` if the rendering is in LaTeX and `"svg"` otherwise.
#' @param ... extra parameters passing to boxplot
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
                         file_type = if (is_latex()) "pdf" else svglite::svglite,
                         ...) {
  if (is.list(x)) {
    if (same_lim & is.null(lim)) {
      lim <- base::range(unlist(x), na.rm=TRUE)
    }

    dots <- listify_args(x, width, height, res,
                         add_label, label_digits,
                         lim, xaxt, yaxt, ann, col, border,
                         dir, file, file_type,
                         lengths = c(1, length(x)))
    return(do.call(Map, c(list(f = spec_boxplot), dots)))
  }

  if (is.null(x)) return(NULL)

  if (is.null(lim)) {
    lim <- base::range(x, na.rm=TRUE)
    lim[1] <- lim[1] - (lim[2] - lim[1]) / 10
    lim[2] <- (lim[2] - lim[1]) / 10 + lim[2]
  }

  if (!dir.exists(dir)) {
    dir.create(dir)
  }

  file_ext <- dev_chr(file_type)
  if (is.null(file)) {
    file <- normalizePath(
      tempfile(pattern = "boxplot_", tmpdir = dir, fileext = paste0(".", file_ext)),
      winslash = "/", mustWork = FALSE)
  }

  graphics_dev(filename = file, dev = file_type,
               width = width, height = height, res = res,
               bg = "transparent")
  curdev <- grDevices::dev.cur()
  on.exit(grDevices::dev.off(curdev), add = TRUE)

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

  grDevices::dev.off(curdev)

  out <- make_inline_plot(
    file, file_ext, file_type,
    width, height, res,
    del = TRUE)
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
#' boxplot, line, scatter and pointrange plots. You can use them together with
#' `column_spec` to generate inline plot in tables. By default, this function
#' will save images in a folder called "kableExtra" and return the address of
#' the file.
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
#' @param frame.plot On/Off for surrounding box (`spec_plot` only). Default
#' is False.
#' @param lwd Line width for `spec_plot`; within `spec_plot`, the `minmax`
#' argument defaults to use this value for `cex` for points. Default is 2.
#' @param pch,cex Shape and size for points (if type is other than "l").
#' @param type Passed to `plot`, often one of "l", "p", or "b", see
#' [graphics::plot.default()] for more details. Ignored when `polymin` is
#' not `NA`.
#' @param polymin Special argument that converts a "line" to a polygon,
#' where the flat portion is this value, and the other side of the polygon
#' is the 'y' value ('x' if no 'y' provided). If `NA` (the default), then
#' this is ignored; otherwise if this is numeric then a polygon is
#' created (and 'type' is ignored). Note that if `polymin` is in the middle
#' of the 'y' values, it will generate up/down polygons around this value.
#' @param minmax,min,max Arguments passed to `points` to highlight minimum
#' and maximum values in `spec_plot`. If `min` or `max` are `NULL`, they
#' default to the value of `minmax`. Set to an empty `list()` to disable.
#' @param dir Directory of where the images will be saved.
#' @param file File name. If not provided, a random name will be used
#' @param file_type Graphic device. Can be character (e.g., `"pdf"`)
#'   or a graphics device function (`grDevices::pdf`). This defaults
#'   to `"pdf"` if the rendering is in LaTeX and `"svg"` otherwise.
#' @param ... extra parameters passing to `plot`
#'
#' @export
spec_plot <- function(x, y = NULL, width = 200, height = 50, res = 300,
                      same_lim = TRUE, xlim = NULL, ylim = NULL,
                      xaxt = 'n', yaxt = 'n', ann = FALSE,
                      col = "lightgray", border = NULL,
                      frame.plot = FALSE, lwd = 2,
                      pch = ".", cex = 2, type = "l", polymin = NA,
                      minmax = list(pch = ".", cex = cex, col = "red"),
                      min = minmax, max = minmax,
                      dir = if (is_latex()) rmd_files_dir() else tempdir(),
                      file = NULL, file_type = if (is_latex()) "pdf" else svglite::svglite,
                      ...) {
  if (is.list(x)) {
    lenx <- length(x)

    if (same_lim) {
      if (is.null(xlim)) {
        xlim <- base::range(unlist(x), na.rm = TRUE)
      }
      if (is.null(ylim) && !is.null(y)) {
        ylim <- base::range(c(unlist(y), polymin), na.rm = TRUE)
      }
    }

    if (is.null(y)) {
      y <- list(y)
    } else if (length(y) != lenx) {
      stop("'x' and 'y' are not the same length")
    }

    dots <- listify_args(x, y = y, width, height, res,
                         xlim, ylim, xaxt, yaxt, ann, col, border, frame.plot,
                         lwd, pch, cex, type, polymin, minmax, min, max,
                         dir, file, file_type,
                         lengths = c(1, lenx))

    return(do.call(Map, c(list(f = spec_plot), dots)))

  }

  if (is.null(x)) return(NULL)

  if (is.null(y) || !length(y)) {
    y <- x
    x <- seq_along(y)
    if (!is.null(xlim) && is.null(ylim)) {
      ylim <- range(c(xlim, polymin), na.rm = TRUE)
      xlim <- range(x)
    }
  }

  if (is.null(xlim)) {
    xlim <- base::range(x, na.rm = TRUE)
  }

  if (is.null(ylim) && !is.null(y)) {
    ylim <- base::range(c(y, polymin), na.rm = TRUE)
  }

  if (is.null(min)) min <- minmax
  if (is.null(max)) max <- minmax

  expand <- c(
    if (!is.null(min) && length(min)) -0.04 else 0,
    if (!is.null(max) && length(max)) +0.04 else 0)
  xlim <- xlim + diff(xlim) * expand
  ylim <- ylim + diff(ylim) * expand

  if (!dir.exists(dir)) {
    dir.create(dir)
  }

  file_ext <- dev_chr(file_type)
  if (is.null(file)) {
    file <- normalizePath(
      tempfile(pattern = "plot_", tmpdir = dir, fileext = paste0(".", file_ext)),
      winslash = "/", mustWork = FALSE)
  }

  graphics_dev(filename = file, dev = file_type,
               width = width, height = height, res = res,
               bg = "transparent")
  curdev <- grDevices::dev.cur()
  on.exit(grDevices::dev.off(curdev), add = TRUE)

  graphics::par(mar = c(0, 0, 0, 0), lwd = lwd)

  dots <- list(...)
  if (!is.na(polymin) && "angle" %in% names(dots)) {
    angle <- dots$angle
    dots$angle <- NULL
  } else angle <- 45

  do.call(graphics::plot,
          c(list(x, y, type = if (is.na(polymin)) type else "n",
                 xlim = xlim, ylim = ylim,
                 xaxt = xaxt, yaxt = yaxt, ann = ann, col = col,
                 frame.plot = frame.plot, cex = cex, pch = pch),
            dots))

  if (!is.na(polymin)) {
    lty <- if ("lty" %in% names(dots)) dots$lty else graphics::par("lty")
    graphics::polygon(c(x[1], x, x[length(x)]), c(polymin, y, polymin),
            border = NA, col = col, angle = angle, lty = lty,
            xpd = if ("xpd" %in% names(dots)) dots$xpd else NA)
  }

  if (!is.null(min) && length(min)) {
    if (!"xpd" %in% names(min)) min$xpd <- NA
    ind <- which.min(y)
    do.call(graphics::points, c(list(x[ind], y[ind]), min))
  }

  if (!is.null(max) && length(max)) {
    if (!"xpd" %in% names(max)) max$xpd <- NA
    ind <- which.max(y)
    do.call(graphics::points, c(list(x[ind], y[ind]), max))
  }

  grDevices::dev.off(curdev)

  out <- make_inline_plot(
    file, file_ext, file_type,
    width, height, res,
    del = TRUE)
  return(out)
}


#' Helper functions to generate inline sparklines
#'
#' @description These functions helps you quickly generate sets of sparkline
#' style plots using base R plotting system. Currently, we support histogram,
#' boxplot, line, scatter and pointrange plots. You can use them together with
#' `column_spec` to generate inline plot in tables. By default, this function
#' will save images in a folder called "kableExtra" and return the address of
#' the file.
#'
#' @param x,xmin,xmax A scalar value or List of scalar values for dot, left
#' and right error bar.
#' @param vline A scalar value for where to draw a vertical line.
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
#' @param col Color for mean dot.
#' @param line_col Color for the line and the error bar.
#' @param cex size of the mean dot and error bar size.
#' @param frame.plot T/F for whether to plot the plot frames.
#' @param dir Directory of where the images will be saved.
#' @param file File name. If not provided, a random name will be used
#' @param file_type Graphic device. Can be character (e.g., `"pdf"`)
#'   or a graphics device function (`grDevices::pdf`). This defaults
#'   to `"pdf"` if the rendering is in LaTeX and `"svg"` otherwise.
#' for HTML output
#' @param ... extra parameters sending to `hist()`
#'
#' @export
spec_pointrange <- function(
  x, xmin, xmax, vline = NULL,
  width = 200, height = 50, res = 300,
  same_lim = TRUE, lim = NULL,
  xaxt = 'n', yaxt = 'n', ann = FALSE,
  col = "red", line_col = "black", cex = 0.3, frame.plot = FALSE,
  dir = if (is_latex()) rmd_files_dir() else tempdir(),
  file = NULL,
  file_type = if (is_latex()) "pdf" else svglite::svglite, ...) {
  if (length(x) > 1) {
    if (same_lim & is.null(lim)) {
      all_range <- c(unlist(xmin), unlist(xmax))
      lim <- base::range(all_range, na.rm=TRUE)
      lim <- lim + c(-0.04 * diff(lim), 0.04 * diff(lim))
    }

    dots <- listify_args(
      x = as.list(x), xmin = as.list(xmin), xmax = as.list(xmax), vline,
      width, height, res,
      lim, xaxt, yaxt, ann, col, line_col, cex, frame.plot,
      dir, file, file_type,
      lengths = c(1, length(x)),
      passthru = c("x", "xmin", "xmax"))
    return(do.call(Map, c(list(f = spec_pointrange), dots)))
  }

  if (is.null(x)) return(NULL)

  if (is.null(lim)) {
    one_range <- unlist(c(xmin, xmax))
    lim <- base::range(one_range, na.rm=TRUE)
    lim <- lim + c(-0.04 * diff(lim), 0.04 * diff(lim))
  }

  if (!dir.exists(dir)) {
    dir.create(dir)
  }

  file_ext <- dev_chr(file_type)
  if (is.null(file)) {
    file <- normalizePath(
      tempfile(pattern = "pointrange_", tmpdir = dir, fileext = paste0(".", file_ext)),
      winslash = "/", mustWork = FALSE)
  }

  graphics_dev(filename = file, dev = file_type,
               width = width, height = height, res = res,
               bg = "transparent")
  curdev <- grDevices::dev.cur()
  on.exit(grDevices::dev.off(curdev), add = TRUE)

  graphics::par(mar = c(0, 0, 0.2, 0), lwd=1,
                ann = ann, xaxt = xaxt, yaxt = yaxt)

  graphics::plot(x, 0, type = "p", pch = ".",
                 xlim = lim, frame.plot = frame.plot)
  graphics::arrows(xmin, 0, xmax, 0, cex / 15, angle = 90, code = 3,
                   col = line_col)
  graphics::points(x, 0, col = col, type = "p", pch = 15, cex = cex)
  if (!is.null(vline)) {
    graphics::abline(v = vline, lty = 3)
  }

  grDevices::dev.off(curdev)

  out <- make_inline_plot(
    file, file_ext, file_type,
    width, height, res,
    del = TRUE)
  return(out)
}
