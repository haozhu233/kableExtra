#' Deprecated
#'
#' @description deprecated
#'
#' @param kable_input Raw LaTeX code to generate a table. It doesn't have to
#' came from `kable` or `kableExtra`.
#' @param filename Character String. If specified, the image will be saved under
#' the specified (path &) name. You don't need to put file format like ".png"
#' here.
#' @param file_format Character String to specify image format, such as `png`,
#' `jpeg`, `gif`, `tiff`, etc. Default is `png`.
#' @param latex_header_includes A character vector of extra LaTeX header stuff.
#' Each element is a row. You can have things like
#' `c("\\\\usepackage{threeparttable}", "\\\\usepackage{icons}")`  You could
#' probably add your language package here if you use non-English text in your
#' table, such as `\\\\usepackage[magyar]{babel}`.
#' @param keep_pdf A T/F option to control if the mid-way standalone pdf should
#' be kept. Default is `FALSE`.
#' @param density Resolution to read the PDF file. Default value is 300, which
#' should be sufficient in most cases.
#' @param keep_tex A T/F option to control if the latex file that is initially created
#' should be kept. Default is `FALSE`.
#'
#' @export
kable_as_image <- function(kable_input, filename = NULL,
                           file_format = "png",
                           latex_header_includes = NULL,
                           keep_pdf = FALSE,
                           density = 300,
                           keep_tex = FALSE) {
  message('kable_as_image is deprecated. Please use save_kable or as_image ',
          'instead.')
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop('kable_as_image requires the magick package, which is not available ',
         'on all platforms. Please get it installed ',
         'via install.packages("magick"). If you are running on Windows, you ',
         'also need to install Ghostscript. Please download it here:',
         'https://ghostscript.com/')
  } else {
    temp_tex <- c(
      "\\documentclass[border=1mm, preview]{standalone}",
      "\\usepackage[active,tightpage]{preview}",
      "\\usepackage{varwidth}",
      "\\usepackage{amssymb, amsmath}",
      "\\usepackage{ifxetex,ifluatex}",
      "\\usepackage{fixltx2e}",
      "\\usepackage{polyglossia}",
      "\\setmainlanguage{$mainlang$}",
      latex_pkg_list(xelatex = TRUE),
      "\\usepackage{graphicx}",
      "\\usepackage{mathspec}",
      "\\usepackage{xltxtra,xunicode}",
      latex_header_includes,
      "\\begin{document}",
      solve_enc(kable_input),
      "\\end{document}"
    )
    temp_tex <- paste(temp_tex, collapse = "\n")
    temp_file <- paste0("table_", format(Sys.time(), "%Y-%m-%d_%H%M%S"))
    writeLines(temp_tex, paste0(temp_file, ".tex"), useBytes = T)
    system(paste0("xelatex -interaction=batchmode ", temp_file, ".tex"))
    temp_file_delete <- paste0(temp_file, c(".tex", ".aux", ".log"))
    if(!keep_tex) {
      unlink(temp_file_delete)
    }

    table_img_pdf <- try(magick::image_read(paste0(temp_file, ".pdf"),
                                            density = density),
                         silent = T)
    if (inherits(table_img_pdf, "try-error")) {
      stop("Ghostscript is required to read PDF on windows. ",
           "Please download it here: https://ghostscript.com/")
    }
    if (!keep_pdf) {
      unlink(paste0(temp_file, ".pdf"))
    }
    table_img <- magick::image_convert(table_img_pdf, file_format)
    if (!is.null(filename)) {
      temp_img <- paste0(filename, ".", file_format)
    } else {
      temp_img <- tempfile(fileext = paste0(".", file_format))
    }
    magick::image_write(table_img, temp_img)

    include_graphics(temp_img)
  }
}
