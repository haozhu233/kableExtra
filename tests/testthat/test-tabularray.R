df <- data.frame(
  car = row.names(mtcars),
  mtcars[, 1:3],
  row.names = NULL
)[1:4,]


test_that("assertions: tabularray does not support vectors for rows", {
    expect_error(
        kbl(df, format = "latex", tabular = "tblr", booktabs = TRUE) |>
            row_spec(2:3, bold = c(TRUE, FALSE, TRUE, TRUE)),
        regexp = "length 1")
    expect_error(
        kbl(df, format = "latex", tabular = "tblr", booktabs = TRUE) |>
            row_spec(2:3, monospace = c(TRUE, FALSE, TRUE, TRUE)),
        regexp = "length 1")
    expect_error(
        kbl(df, format = "latex", tabular = "tblr", booktabs = TRUE) |>
            row_spec(2:3, underline = c(TRUE, FALSE, TRUE, TRUE)),
        regexp = "length 1")
    expect_error(
        kbl(df, format = "latex", tabular = "tblr", booktabs = TRUE) |>
            column_spec(2:3, bold = c(TRUE, FALSE, TRUE, TRUE)),
        regexp = "length 1")
    expect_error(
        kbl(df, format = "latex", tabular = "tblr", booktabs = TRUE) |>
            column_spec(2:3, monospace = c(TRUE, FALSE, TRUE, TRUE)),
        regexp = "length 1")
    expect_error(
        kbl(df, format = "latex", tabular = "tblr", booktabs = TRUE) |>
            column_spec(2:3, underline = c(TRUE, FALSE, TRUE, TRUE)),
        regexp = "length 1")
})


test_that('basic', {
    expect_snapshot(kbl(df, format = "latex", tabular = "tblr", booktabs = TRUE))

    expect_snapshot(
        kbl(df, format = "latex", tabular = "tblr", booktabs = TRUE) |> 
        row_spec(2:3, bold = TRUE, background = "pink") 
    )

    expect_snapshot(
        kbl(df, format = "latex", booktabs = TRUE) |> 
        row_spec(2:3, bold = TRUE, background = "pink")
    )
})

test_that('style and color', {
    expect_snapshot(
      kbl(df, format = "latex", tabular = "tblr", booktabs = TRUE) |>
      row_spec(2:3, bold = TRUE) |>
      column_spec(1, strikeout = TRUE)
    )

    expect_snapshot(
      kbl(df, format = "latex", tabular = "tblr", booktabs = TRUE) |>
      column_spec(1, color = "green!30!black") |>
      row_spec(2:3, background = "azure9")
    )

    expect_snapshot(
      kbl(df, format = "latex", tabular = "tblr", booktabs = TRUE) |>
      row_spec(2:3, background = "azure9") |>
      column_spec(1, color = "green!30!black", strikeout = TRUE)
    )
})



test_that('tabularray outer options', {
    expect_snapshot(
        kbl(df, format = "latex", tabular = "talltblr", booktabs = TRUE) |>
        kable_styling(latex_options = list(tabularray_outer = c(
            "caption = {A caption.}",
            "label = {tab:mytable}",
            "headsep = 12pt"
        )))
    )
})


test_that('lines', {
    expect_snapshot(
        kbl(df, format = "latex", tabular = "tblr", booktabs = TRUE) |>
        row_spec(2, background = "pink, ht=3.5em, valign=f")
    )

    expect_snapshot(
        kbl(df, format = "latex", tabular = "tblr", vline = "", linesep = "",
            toprule = "", midrule = "", bottomrule = "") |>
        kable_styling(
            latex_options = list(tabularray_inner = c(
                "hlines={dash=dotted, fg=brown6}",
                "vlines={dash=dashed, fg=green4, wd=2pt}"
            ))
        )
    )

    expect_snapshot(
        kbl(df, format = "latex", tabular = "tblr", vline = "",
            toprule = "", midrule = "", bottomrule = "", linesep = "") |>
        kable_styling(
            latex_options = list(tabularray_inner = c(
                "hline{1-6}={dash=solid, fg=brown6}",
                "vline{2,3}={dash=dotted, fg=green4}"
            ))
        )
    )
})


test_that('add_header_above', {
    expect_snapshot(
        mtcars[1:4, 1:5] |>
        kbl(format = "latex", tabular = "tblr", align = "c", booktabs = TRUE) |> 
        add_header_above(
            c(" " = 1, "\\alpha" = 2, "\\beta" = 3),
            escape = FALSE) |>
        add_header_above(
            c( "First Three" = 3, " " = 1, "Penultimate" = 1, " " = 1),
            italic = TRUE)
    )
})


test_that('issue #616', {
    expect_snapshot(
        mtcars %>%
        head(n = 10) %>%
        kbl(format = "latex", tabular = "tblr") %>%
        kable_styling(latex_options = "HOLD_position") |>
        row_spec(seq(1, 10, by = 2), background = "gray8, ht=1cm") |>
        row_spec(seq(2, 10, by = 2), background = "white, ht=1cm")
    )
})


test_that('issue #636', {
    set.seed(1024)
    paint <- function(x) {
      col <- ifelse(x < 0.5, "yellow", "red")
      sapply(seq_along(x), function(i) cell_spec(
        formatC(x[i], format = "f", digits = 2),
        background = col[i], format = "tblr")
      )
    }
    DF <- data.frame(
      V1 = sample(letters,10,T),
      V2 = abs(rnorm(10)),
      V3 = abs(rnorm(10)))
    DF[,-1] = lapply(DF[,-1], paint)

    expect_snapshot(
        kbl(DF, format = "latex", tabular = "tblr", digits = 2, escape = FALSE) |>
        kable_styling(latex_options = "striped")
    )
})


test_that('issue #738', {
    t <- head(cars)
    t$dist <- cell_spec(t$dist, background = "red", format = "tblr")
    expect_snapshot(
        kbl(t, format = "latex", tabular = "tblr", escape = FALSE) |>
        kable_styling(latex_options = c("striped"))
    )
})
