library(testthat)
library(knitr)

dt <- mtcars[1:5, 1:8]


# ---- styling -----------------------------------------------------------------
testthat::context("styling")

test_that("dry_run", {
  observed <- dt[1:3] %>%
    kable(format = "latex", booktabs = T, caption = "xxx") %>%
    kable_styling(latex_options = c("striped", "hold_position"), font_size = 6, position = "float_left")
  dput(observed)

  expected <- structure("\\rowcolors{2}{gray!6}{white}\n\\begin{wraptable}{l}{6.6cm}\n\n\\caption{\\label{tab:}xxx}\n\\centering\n\\fontsize{6}{8}\\selectfont\n\\begin{tabular}[t]{lrrr}\n\\hiderowcolors\n\\toprule\n  & mpg & cyl & disp\\\\\n\\midrule\n\\showrowcolors\nMazda RX4 & 21.0 & 6 & 160\\\\\nMazda RX4 Wag & 21.0 & 6 & 160\\\\\nDatsun 710 & 22.8 & 4 & 108\\\\\nHornet 4 Drive & 21.4 & 6 & 258\\\\\nHornet Sportabout & 18.7 & 8 & 360\\\\\n\\bottomrule\n\\end{tabular}\n\\end{wraptable}\n\\rowcolors{2}{white}{white}", format = "latex", class = "knitr_kable", kable_meta = structure(list(
    tabular = "tabular", booktabs = TRUE, align = "lrrr", valign = "[t]",
    ncol = 4L, nrow = 6, colnames = c(" ", "mpg", "cyl", "disp"
    ), rownames = c("", "Mazda", "Mazda", "Datsun", "Hornet",
                    "Hornet"), caption = "\\label{tab:}xxx", caption.short = NULL,
    contents = c("  & mpg & cyl & disp", "Mazda RX4 & 21.0 & 6 & 160",
                 "Mazda RX4 Wag & 21.0 & 6 & 160", "Datsun 710 & 22.8 & 4 & 108",
                 "Hornet 4 Drive & 21.4 & 6 & 258", "Hornet Sportabout & 18.7 & 8 & 360"
    ), centering = TRUE, table_env = TRUE, align_vector = c("l",
                                                            "r", "r", "r"), align_vector_origin = c("l", "r", "r", "r"
                                                            ), valign2 = "\\[t\\]", valign3 = "t", begin_tabular = "\\\\begin\\{tabular\\}\\[t\\]",
    end_tabular = "\\\\end\\{tabular\\}"), .Names = c("tabular",
                                                      "booktabs", "align", "valign", "ncol", "nrow", "colnames", "rownames",
                                                      "caption", "caption.short", "contents", "centering", "table_env",
                                                      "align_vector", "align_vector_origin", "valign2", "valign3",
                                                      "begin_tabular", "end_tabular")))

  expect_equal(observed, expected)
})
