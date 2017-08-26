#' @export
kable_as_image_latex <- function(kable_input, width = NULL,
                                 latex_header_includes = NULL,
                                 keep_pdf = FALSE) {
  temp_tex <- c(
    "\\documentclass[border=1mm, preview]{standalone}",
    "\\usepackage[active,tightpage]{preview}",
    "\\usepackage{varwidth}",
    "\\usepackage{booktabs}",
    "\\usepackage{longtable}",
    "\\usepackage{array}",
    "\\usepackage{multirow}",
    "\\usepackage[table]{xcolor}",
    "\\usepackage{wrapfig}",
    "\\usepackage{colortbl}",
    "\\usepackage{graphicx}",
    latex_header_includes,
    "\\begin{document}",
    as.character(kable_input),
    "\\end{document}"
  )
  temp_tex <- paste(temp_tex, collapse = "\n")
  temp_file <- paste0("table_", format(Sys.time(), "%Y-%m-%d_%H:%M:%S"))
  write_file(temp_tex, paste0(temp_file, ".tex"))
  system(paste0("xelatex -interaction=batchmode ", temp_file, ".tex"))
  temp_file_delete <- paste0(temp_file, c(".tex", ".aux", ".log"))
  unlink(temp_file_delete)

  table_img_pdf <- image_read(paste0(temp_file, ".pdf"), density = 300)
  if (!keep_pdf) {
    unlink(paste0(temp_file, ".pdf"))
  }
  table_img <- image_convert(table_img_pdf, "png")
  if (!is.null(width)) {
    table_img <- image_scale(table_img, as.character(300*width))
  }
  temp_img <- tempfile(fileext = ".png")
  image_write(table_img, temp_img)

  include_graphics(temp_img)
}
