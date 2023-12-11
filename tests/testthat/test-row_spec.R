# TODO: use system.file() instead of here::here()
test_that("Rmarkdown  compilation", {
    tmp <- tempfile(fileext = ".Rmd")
    file.copy(here::here("inst/rmarkdown/test_row_spec.Rmd"), tmp)
    compile <- try(rmarkdown::render(tmp, quiet = TRUE), silent = TRUE)
    expect_false(inherits(compile, "try-error"))
})


test_that("LaTeX: basic argument tests", {
    df <- data.frame(1:4, 4:7)

    expect_snapshot(
        kbl(df, format = "latex") |>
            row_spec(3,
                bold = TRUE,
                monospace = TRUE,
                underline = TRUE,
                italic = TRUE
            )
    )

    expect_snapshot(
        kbl(df, format = "latex") |>
            row_spec(3, angle = 45)
    )

    expect_snapshot(
        kbl(df, format = "latex") |>
            row_spec(3, font_size = 10)
    )

    expect_snapshot(
        kbl(df, format = "latex") |>
            row_spec(3, color = "blue", background = "pink")
    )

    expect_snapshot(
        kbl(df, format = "latex", booktabs = TRUE) |>
            kable_classic() |>
            row_spec(3, hline_after = TRUE)
    )

    df <- data.frame(a = c("ab", "abc"), b = c("abcd", "abcde"))
    expect_snapshot(
        kbl(df, format = "latex") |>
            row_spec(1, align = "r") |>
            row_spec(2, align = "c")
    )
})


test_that("issue #582: RMD kable styling repeates rows when rendering striped table to LaTeX in some data combination", {
    dfWithDot <- data.frame(
        A = c("A1", "A2", "A."),
        B1 = c("A1B1", "A2B1", "A.B1"),
        B2 = c("A1B2", "A2B2", "A.B2"),
        B3 = c("A1B3", "A2B3", "A.B3"),
        B. = c("A1B.", "A2B.", "A.B."))
    expect_snapshot(
        kbl(dfWithDot, format = "latex") |>
            kable_styling(latex_options = "striped")
    )
})


test_that("awesome table example", {
    set.seed(1024)
    collapse_rows_dt <- data.frame(
        C1 = c(rep("a", 10), rep("b", 5)),
        C2 = c(rep("c", 7), rep("d", 3), rep("c", 2), rep("d", 3)),
        C3 = 1:15,
        C4 = sample(c(0, 1), 15, replace = TRUE))
    expect_snapshot(
        kbl(collapse_rows_dt[-1], align = "c", booktabs = T, format = "latex") %>%
            column_spec(1, bold = T, width = "5em") %>%
            row_spec(c(1:7, 11:12) - 1, extra_latex_after = "\\rowcolor{gray!6}") %>%
            collapse_rows(1, latex_hline = "none")
    )
})



test_that("Issue #576: string font_size in html", {
    expect_snapshot(
        data.frame(a = c("foo", "bar", "foo", "bar", "foo", "bar")) %>%
            kbl() %>%
            row_spec(3, font_size = "xx-large") |>
            cat()
    )
})