context("add_indent")

test_that("add_indent can add to 1 row", {
  observed <- kable(mtcars[1:4, 1:3], "latex") %>%
    add_indent(1) %>%
    as.character()
  expected <- "\n\\begin{tabular}{l|r|r|r}\n\\hline\n  & mpg & cyl & disp\\\\\n\\hline\n\\hspace{1em}Mazda RX4 & 21.0 & 6 & 160\\\\\n\\hline\nMazda RX4 Wag & 21.0 & 6 & 160\\\\\n\\hline\nDatsun 710 & 22.8 & 4 & 108\\\\\n\\hline\nHornet 4 Drive & 21.4 & 6 & 258\\\\\n\\hline\n\\end{tabular}"
  expect_equal(observed, expected)
})

test_that("add_indent can be added multiple times.", {
  observed <- kable(mtcars[1:4, 1:3], "latex") %>%
    add_indent(1:3) %>%
    add_indent(1) %>%
    as.character()
  expected <- "\n\\begin{tabular}{l|r|r|r}\n\\hline\n  & mpg & cyl & disp\\\\\n\\hline\n\\hspace{1em}\\hspace{1em}Mazda RX4 & 21.0 & 6 & 160\\\\\n\\hline\n\\hspace{1em}Mazda RX4 Wag & 21.0 & 6 & 160\\\\\n\\hline\n\\hspace{1em}Datsun 710 & 22.8 & 4 & 108\\\\\n\\hline\nHornet 4 Drive & 21.4 & 6 & 258\\\\\n\\hline\n\\end{tabular}"
  expect_equal(observed, expected)
})


