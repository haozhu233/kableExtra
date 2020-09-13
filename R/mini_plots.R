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

  if (is.null(x)) return(NULL)

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

  if (is.null(x)) return(NULL)

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
#' @param pch,cex Shape and size for points (if type is other than "l").
#' @param type Passed to `plot`, often one of "l", "p", or "b", see
#' [graphics::plot.default()] for more details. Ignored when 'polymin' is
#' not 'NA'.
#' @param polymin Special argument that converts a "line" to a polygon,
#' where the flat portion is this value, and the other side of the polygon
#' is the 'y' value ('x' if no 'y' provided). If 'NA' (the default), then
#' this is ignored, otherwise if this is numeric then a polygon is
#' created instead (and 'type' is ignored). Note that if 'polymin' is in
#' the middle of the 'y' values, it will generate up/down polygons.
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
                      pch = ".", cex = 0.1, type = "l", polymin = NA,
                      minmax = list(pch = ".", cex = lwd, col = "red"),
                      min = minmax, max = minmax,
                      dir = if (is_latex()) rmd_files_dir() else tempdir(),
                      file = NULL, file_type = if (is_latex()) "png" else "svg", ...) {
  if (is.list(x)) {
    lenx <- length(x)

    if (same_lim) {
      if (is.null(xlim)) {
        xlim <- lapply(x, function(z) base::range(c(z, if (is.null(y)) polymin), na.rm = TRUE))
      }
      if (is.null(ylim) && !is.null(y)) {
        if (is.list(y)) {
          ylim <- lapply(y, function(z) base::range(c(z, polymin), na.rm = TRUE))
        } else {
          ylim <- base::range(c(y, polymin), na.rm = TRUE)
        }
      }
    }
    if (is.null(y)) {
      y <- replicate(lenx, NULL, simplify = FALSE)
    } else if (!is.list(y) || lenx != length(y)) {
      stop("'x' and 'y' are not the same length")
    }

    # any of the arguments can be per-plot controlling, but an arg
    # that is normally not length-1 may be recycled (incorrectly) by
    # Map, so we have to listify them if not already lists;
    # enforce a restriction of recycling only length 1 or lenx

    # first, start with the literals (x,y)
    # (same_lim is not a factor anymore)
    dots <- list(x = x, y = y)

    # second, we know these args are likely to be vectors (length > 1)
    # or lists, so we have to handle them carefully and double-list if
    # present
    notlen1vec <- list(xlim = xlim, ylim = ylim)
    dots <- c(dots, Map(
      function(L, nm) {
        if (is.null(L)) return(list(NULL))
        if (!is.list(L)) return(list(L))
        if (length(L) == lenx) return(L)
        stop("length of '", nm, "' must be 1 or the same length as 'x'")
      }, notlen1vec, names(notlen1vec)))

    # these are special ... they are lists which may need to be
    # nested, and we can't pass NULL since that may default to the
    # actual values instead of the intended
    notlen1lst <- list(minmax = minmax, min = min, max = max)
    dots <- c(dots, Map(
      function(L, nm) {
        if (is.null(L)) return(list(NULL))
        if (!length(L)) return(list(list()))
        if (!is.list(L[[1]])) return (list(L))
        if (length(L) == lenx) return(L)
        stop("length of '", nm, "' must be 1 or the same length as 'x'")
      }, notlen1lst, names(notlen1lst)))

    # last, all remaining arguments that we don't already know about
    # are length-1, so can be easily listified; using 'formals()'
    # allows us to not hard-code all params
    len1args <- mget(setdiff(names(formals()),
                             c(names(dots), "same_lim", "x", "y", "...")))
    dots <- c(dots, Map(
      function(V, nm) {
        if (is.null(V)) return(list(NULL))
        if (!length(V) %in% c(1L, lenx)) {
          stop("length of '", nm, "' must be 1 or the same length as 'x'")
        }
        V
      }, len1args, names(len1args)))

    return(do.call(Map, c(list(f = spec_line), dots)))

  }

  if (is.null(x)) return(NULL)

  if (is.null(y) || !length(y)) {
    y <- x
    x <- seq_along(y)
    if (!is.null(xlim) && is.null(ylim)) {
      ylim <- xlim
      xlim <- range(x)
    }
  }

  if (is.null(xlim)) {
    xlim <- base::range(x)
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
                 frame.plot = frame.plot, xpd = NA,
                 cex = cex, pch = pch # in case of type="p" or similar
                 ), dots))

  if (!is.na(polymin)) {
    lty <- if ("lty" %in% names(dots)) dots$lty else graphics::par("lty")
    polygon(c(x[1], x, x[length(x)]), c(polymin, y, polymin),
            border = NA, col = col, angle = angle,
            lty = lty,
            xpd = NA)
  }

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
