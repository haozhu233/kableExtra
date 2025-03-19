files <- list.files("~/svn/MyR/kableExtra2/tests/visual_tests", pattern="Rmd$", full.names=TRUE)
files <- files[!grepl("html", files)]
files <- files[!grepl("child", files)]
files <- files[!grepl("perm.Rmd", files)]
files <- files[!grepl("footnote", files)]
files <- files[!grepl("as_image", files)]
for (f in files) {
  oldf <- sub("kableExtra2", "kableExtra", f)
  rmarkdown::render(oldf, output_dir="~/svn/MyR/kableExtra2/tests/visual_tests/old", output_format = "latex_document")
  rmarkdown::render(f, output_dir="~/svn/MyR/kableExtra2/tests/visual_tests/new", output_format = "latex_document")
}
