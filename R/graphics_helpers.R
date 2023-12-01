#' Helper functions to use various graphics devices
#'
#' These helper functions generalize the use of strings (e.g.,
#' `"svg"`, `"pdf"`) or graphic device functions (e.g.,
#' `grDevices::svg`, `grDevices::pdf`) for in-table plots.
#'
#' @param filename Passed through to the graphics device.
#' @param width,height Plot dimensions in pixels.
#' @param res The resolution of the plot; default is 300.
#' @param ... extra parameters passing to the graphics-device function.
#' @param dev Character (e.g., "svg", "pdf") or function (e.g.,
#' `grDevices::svg`, `grDevices::pdf`).
#' @name graphics_helpers
NULL

#' @describeIn graphics_helpers Generalize `res` and `filename` across device functions
#' @details
#' - `graphics_dev` generalizes the use of 'res' and plot dimensions
#'   across graphic devices. Raster-based devices (e.g., 'png',
#'   'jpeg', 'tiff', 'bmp') tend to use 'res' and the width/height
#'   units default to pixels. All other devices (e.g., 'pdf', 'svg')
#'   tend to use inches as the default units for width/height, and
#'   error when 'res' is provided.
#'
#'   The current heuristic is the look for the 'res' argument in the
#'   function's formals; if that is present, then it is assumed that
#'   the default units are in pixels, so 'width', 'height', and 'res'
#'   are passed through unmodified. If 'res' is not present, then
#'   'width' and 'height' are converted from pixels to inches, and
#'   'res' is not passed to the function
#'
#'   Another purpose of this function is to generalize the different
#'   graphic functions' use of 'file=' versus 'filename='.
#' @return `graphics_dev`: nothing, a plot device is opened
graphics_dev <- function(filename, width, height, res, ..., dev) {
  dev <- match.fun(dev)
  frmls <- names(formals(dev))
  dots <- list(...)
  if ("res" %in% frmls) {
    dots <- c(dots, list(width = width, height = height, res = res))
  } else {
    dots <- c(dots, list(width = width / res, height = height / res))
  }
  filenames <- c("file", "filename")
  found <- na.omit(match(frmls, filenames))[1]
  if (length(found)) {
    dots <- c(dots, setNames(filename, filenames[ found ]))
  } else {
    stop("could not find a 'file' argument in graphics dev")
  }
  do.call(dev, dots)
}

#' @describeIn graphics_helpers Determine if plot device is svg-like
#' @details
#' - `is_svg` determines if the plot device is svg-like, typically one
#'   of `"svg", `grDevices::svg`, or `svglite::svglite`
#' @return 'is_svg': logical
is_svg <- function(dev) {
  if (is.character(dev)) {
    return(grepl("svg", dev))
  }
  if (is.function(dev)) {
    return(any(sapply(formals(dev), function(f) {
      tryCatch(any(grepl("svg", as.character(f))),
               error = function(e) FALSE)
    })))
  }
  stop("unrecognized graphics 'dev': ", paste(class(dev), collapse = ","))
}

#' @describeIn graphics_helpers Determine filename extension
#' @details
#'
#' - `dev_chr` determines the filename extension for the applicable
#'   plot function; when `dev` is a string, then it is returned
#'   unchanged; when `dev` is a function, the formals of the function
#'   are checked for clues (i.e., default value of a `file=` argument)
#' @return `dev_chr`: character
#' @importFrom tools file_ext
dev_chr <- function(dev) {
  ext <- ""
  if (is.character(dev)) {
    ext <- if (dev == "svglite") "svg" else dev
  } else if (is.function(dev)) {
    frmls <- formals(dev)
    filearg <- grep("^file(name)?$", names(frmls), value = TRUE)
    if (length(filearg)) {
      ext <- grep("\\.[[:alpha:]]+$", unlist(sapply(frmls[filearg], as.character)),
                  value = TRUE)
      ext <- unique(tools::file_ext(ext))[1]
    }
  }
  if (is.na(ext) || !nzchar(ext)) {
    warning("could not determine filename extension from graphic device")
    ext <- ""
  }
  return(ext)
}

#' Combine file (or svg text) and parameters into a `kableExtraInlinePlots` object
#'
#' @param filename Passed through to the graphics device.
#' @param file_ext Character, something like "png".
#' @param dev Character (e.g., "svg", "pdf") or function (e.g.,
#' @param width,height Plot dimensions in pixels.
#' @param res The resolution of the plot; default is 300.
#' @param del If the file is svg-like, then the default action is to
#'   read the file into an embedded SVG object; once done, the file is
#'   no longer used. The default action is to delete this file early,
#'   set this to 'FALSE' to keep the file.
#' @return list object, with class `kableExtraInlinePlots`
make_inline_plot <- function(filename, file_ext, dev,
                             width, height, res,
                             del = TRUE) {
  if ((is_svg(file_ext) || is_svg(dev))) {
    svg_xml <- xml2::read_xml(filename)
    svg_text <- as.character(svg_xml)
    if (del) {
      unlink(filename)
      filename <- character(0)
    }
  } else {
    if (!is_latex()) {
      filename <- paste0("file:///", normalizePath(filename, winslash = "/"))
    }
    svg_text <- NULL
  }
  out <- list(path = filename, dev = file_ext, type = "line",
              width = width, height = height, res = res,
              svg_text = svg_text)
  class(out) <- c("kableExtraInlinePlots", "list")
  return(out)
}

#' Convert arguments for a single call into Map-able args
#'
#' @param ... Arbitrary arguments to be possibly converted into lists
#'   of arguments.
#' @param lengths Allowable lengths of the arguments, typically 1 and
#'   the length of the main variable (e.g., "x"). If  `NA` (default),
#'   it is not enforced.
#' @param passthru Character vector of variables to pass through with
#'   no conversion to lists of values. Extra names (not provided in
#'   `...`) are ignored.
#' @param notlen1vec Character vector of variables that are known to
#'   be length over 1 for a single plot call, so it will always be
#'   list-ified and extra care to ensure it is grouped correctly.
#'   Extra names (not provided in `...`) are ignored.
#' @param notlen1lst Character vector of variables that are lists, so
#'   the inner list length is not checked/enforced. (For example, if a
#'   single plot call takes an argument `list(a=1,b=2,d=3)` and the
#'   multi-data call creates three plots, then a naive match might
#'   think that the first plot would get `list(a=1)`, second plot gets
#'   `list(b=2)`, etc. Adding that list-argument to this 'notlen1lst'
#'   will ensure that the full list is passed correctly.) Extra names
#'   (not provided in `...`) are ignored.
#' @param ignore Character vector of variables to ignore, never
#'   returned. (Generally one can control this by not adding the
#'   variable in the first place, but having this here allows some
#'   sanity checks and/or programmatic usage.)
#' @return list, generally a list of embedded lists
listify_args <- function(..., lengths = NA,
                         passthru = c("x", "y"),
                         notlen1vec = c("lim", "xlim", "ylim"),
                         notlen1lst = c("minmax", "min", "max"),
                         ignore = c("same_lim")) {
  indots <- list(...)
  dotnms <- sapply(match.call(expand.dots=FALSE)$..., deparse)
  neednames <- if (is.null(names(indots))) {
    rep(TRUE, length(indots))
  } else !nzchar(names(indots))
  if (any(neednames)) {
    names(indots)[ neednames ] <- dotnms[ neednames ]
  }
  dots <- indots[ intersect(names(indots), passthru) ]

  # these are elements that are not typically length-1, so we need to
  # listify slightly differently
  nms <- intersect(names(indots), notlen1vec)
  if (length(nms)) {
    dots <- c(dots, Map(
      function(L, nm) {
        if (is.null(L)) return(list(NULL))
        if (!is.list(L)) return(list(L))
        if ((length(lengths) == 1 && is.na(lengths)) || length(L) %in% lengths) return(L)
        stop("length of '", nm, "' must be one of: ", paste(lengths, collapse = " or "))
      }, indots[ nms ], nms))
  }

  # these are a little special in that the argument must be a list
  # (regardless of its internal length)
  nms <- intersect(names(indots), notlen1lst)
  if (length(nms)) {
    dots <- c(dots, Map(
      function(L, nm) {
        if (is.null(L)) return(list(NULL))
        if (!length(L)) return(list(list()))
        if (!is.list(L[[1]])) return (list(L))
        if ((length(lengths) == 1 && is.na(lengths)) || length(L) %in% lengths) return(L)
        stop("length of '", nm, "' must be one of: ", paste(lengths, collapse = " or "))
      }, indots[ nms ], nms))
  }

  # the remainder, those that we don't know about explicitly and are
  # not intentionally ignored
  nms <- setdiff(names(indots), c(passthru, notlen1vec, notlen1lst, ignore))
  if (length(nms)) {
    dots <- c(dots, Map(
      function(V, nm) {
        if (is.null(V)) return(list(NULL))
        if (is.function(V)) return(list(V))
        if ((length(lengths) == 1 && is.na(lengths)) || length(V) %in% lengths) return(V)
        stop("length of '", nm, "' must be one of: ", paste(lengths, collapse = " or "))
      }, indots[ nms ], nms))
  }

  dots
}
