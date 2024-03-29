% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mini_plots.R
\name{spec_plot}
\alias{spec_plot}
\title{Helper functions to generate inline sparklines}
\usage{
spec_plot(
  x,
  y = NULL,
  width = 200,
  height = 50,
  res = 300,
  same_lim = TRUE,
  xlim = NULL,
  ylim = NULL,
  xaxt = "n",
  yaxt = "n",
  ann = FALSE,
  col = "lightgray",
  border = NULL,
  frame.plot = FALSE,
  lwd = 2,
  pch = ".",
  cex = 2,
  type = "l",
  polymin = NA,
  minmax = list(pch = ".", cex = cex, col = "red"),
  min = minmax,
  max = minmax,
  dir = if (is_latex()) rmd_files_dir() else tempdir(),
  file = NULL,
  file_type = if (is_latex()) "pdf" else svglite::svglite,
  ...
)
}
\arguments{
\item{x, y}{Vector of values or List of vectors of values. y is optional.}

\item{width}{The width of the plot in pixel}

\item{height}{The height of the plot in pixel}

\item{res}{The resolution of the plot. Default is 300.}

\item{same_lim}{T/F. If x is a list of vectors, should all the plots be
plotted in the same range? Default is True.}

\item{xlim, ylim}{Manually specify plotting range in the form of
\code{c(0, 10)}.}

\item{xaxt}{On/Off for xaxis text}

\item{yaxt}{On/Off for yaxis text}

\item{ann}{On/Off for annotations (titles and axis titles)}

\item{col}{Color for the fill of the histogram bar/boxplot box.}

\item{border}{Color for the border.}

\item{frame.plot}{On/Off for surrounding box (\code{spec_plot} only). Default
is False.}

\item{lwd}{Line width for \code{spec_plot}; within \code{spec_plot}, the \code{minmax}
argument defaults to use this value for \code{cex} for points. Default is 2.}

\item{pch, cex}{Shape and size for points (if type is other than "l").}

\item{type}{Passed to \code{plot}, often one of "l", "p", or "b", see
\code{\link[graphics:plot.default]{graphics::plot.default()}} for more details. Ignored when \code{polymin} is
not \code{NA}.}

\item{polymin}{Special argument that converts a "line" to a polygon,
where the flat portion is this value, and the other side of the polygon
is the 'y' value ('x' if no 'y' provided). If \code{NA} (the default), then
this is ignored; otherwise if this is numeric then a polygon is
created (and 'type' is ignored). Note that if \code{polymin} is in the middle
of the 'y' values, it will generate up/down polygons around this value.}

\item{minmax, min, max}{Arguments passed to \code{points} to highlight minimum
and maximum values in \code{spec_plot}. If \code{min} or \code{max} are \code{NULL}, they
default to the value of \code{minmax}. Set to an empty \code{list()} to disable.}

\item{dir}{Directory of where the images will be saved.}

\item{file}{File name. If not provided, a random name will be used}

\item{file_type}{Graphic device. Can be character (e.g., \code{"pdf"})
or a graphics device function (\code{grDevices::pdf}). This defaults
to \code{"pdf"} if the rendering is in LaTeX and \code{"svg"} otherwise.}

\item{...}{extra parameters passing to \code{plot}}
}
\description{
These functions helps you quickly generate sets of sparkline
style plots using base R plotting system. Currently, we support histogram,
boxplot, line, scatter and pointrange plots. You can use them together with
\code{column_spec} to generate inline plot in tables. By default, this function
will save images in a folder called "kableExtra" and return the address of
the file.
}
