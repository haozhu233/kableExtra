#' Helper functions to generate inline sparklines
#'
#' @description These functions helps you quickly generate sets of sparkline
#' style plots using base R plotting system. Currently, we support histogram
#' and boxplot. You can use them together with `column_spec` to
#' generate inline plot in tables. By default, this function will save images
#' in a folder called "kableExtra" and return the address of the file.
#'
#' @param x Vector of values or List of vectors of values.
#' @param width The width of the plot in pixel
#' @param height The height of the plot in pixel
#' @param res The resolution of the plot. Default is 300.
#' @param same_lim T/F. If x is a list of vectors, should all the plots be
#' plotted in the same range? Default is True.
#' @param lim Manually specify plotting range in the form of `c(0, 10)`.
#' @param xaxt On/Off for xaxis text
#' @param yaxt On/Off for yaxis text
#' @param ann On/Off for annotations (titles and axis titles)
#' @param col Color for the fill of the histogram bar/boxplot box.
#' @param border Color for the border.
#' @param dir Directory of where the images will be saved.
#' @param file File name. If not provided, a random name will be used
#' @param file_type Graphic device. Support `png` or `svg`. SVG is recommended
#' for HTML output
#' @param add_label For boxplot. T/F to add labels for min, mean and max.
#' @param label_digits If T for add_label, rounding digits for the label.
#' Default is 2.
#' @param boxlty Boxplot - box boarder type
#' @param medcol Boxplot - median line color
#' @param medlwd Boxplot - median line width
#'
#' @inheritParams graphics::hist
#' @inheritParams graphics::boxplot
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
    file <- tempfile("hist", dir, paste0('.', file_type))
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

#' @rdname spec_hist
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
    file <- tempfile("hist", dir, paste0('.', file_type))
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

