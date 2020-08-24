mini_hist <- function(x, width = 60, height = 20,
                      dir = "kableExtra", file = NULL,
                      xaxt = 'n', yaxt = 'n', ann = FALSE, col = "gray",
                      ...) {
  if (!dir.exists(dir)) {
    dir.create(dir)
  }

  if (is.null(file)) {
    file <- tempfile("hist", dir, ".png")
  }

  grDevices::png(filename = file, width = width, height = height,
                 bg = 'transparent')
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::hist(x, xaxt = xaxt, yaxt = yaxt, ann = ann, col = col, ...)
  grDevices::dev.off()
  return(file)
}
