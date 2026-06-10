# Characterization tests for kable_styling(full_width = TRUE) in LaTeX.
#
# full_width = TRUE historically converted tables to the tabu/longtabu
# environments. The tabu package is unmaintained and CRAN flagged it, so
# these tests pin down the behavior of every feature that interacts with
# full_width before (and after) migrating to tabularx/xltabular.

test_that("LaTeX full_width: basic table", {
    df <- data.frame(a = 1:4, b = 4:7)

    expect_snapshot(
        kbl(df, format = "latex") |>
            kable_styling(full_width = TRUE)
    )

    expect_snapshot(
        kbl(df, format = "latex", booktabs = TRUE) |>
            kable_styling(full_width = TRUE)
    )
})

test_that("LaTeX full_width: alignment is mapped onto X columns", {
    df <- data.frame(a = 1:4, b = 4:7, c = letters[1:4])

    expect_snapshot(
        kbl(df, format = "latex", align = "lcr") |>
            kable_styling(full_width = TRUE)
    )
})

test_that("LaTeX full_width: longtable", {
    df <- data.frame(a = 1:4, b = 4:7)

    expect_snapshot(
        kbl(df, format = "latex", longtable = TRUE, booktabs = TRUE) |>
            kable_styling(full_width = TRUE)
    )

    expect_snapshot(
        kbl(df, format = "latex", longtable = TRUE, booktabs = TRUE,
            caption = "A long table") |>
            kable_styling(full_width = TRUE,
                          latex_options = "repeat_header")
    )
})

test_that("LaTeX full_width: striped and caption", {
    df <- data.frame(a = 1:4, b = 4:7)

    expect_snapshot(
        kbl(df, format = "latex", booktabs = TRUE, caption = "Hello") |>
            kable_styling(full_width = TRUE, latex_options = "striped")
    )
})

test_that("LaTeX full_width: downstream spec functions still work", {
    df <- data.frame(a = 1:4, b = 4:7, c = letters[1:4])

    expect_snapshot(
        kbl(df, format = "latex", booktabs = TRUE) |>
            kable_styling(full_width = TRUE) |>
            column_spec(1, width = "3cm") |>
            row_spec(2, bold = TRUE)
    )

    expect_snapshot(
        kbl(df, format = "latex", booktabs = TRUE) |>
            kable_styling(full_width = TRUE) |>
            add_header_above(c(" " = 1, "Group" = 2))
    )

    expect_snapshot(
        kbl(df, format = "latex", booktabs = TRUE) |>
            kable_styling(full_width = TRUE) |>
            footnote(general = "A general footnote.")
    )

    expect_snapshot(
        kbl(df, format = "latex", booktabs = TRUE) |>
            kable_styling(full_width = TRUE) |>
            pack_rows("Group 1", 1, 2)
    )
})

test_that("LaTeX full_width: structural invariants", {
    # These assertions are environment-agnostic on purpose: they must hold
    # both for the legacy tabu output and for its replacement.
    df <- data.frame(a = 1:4, b = 4:7)

    out <- kbl(df, format = "latex") |>
        kable_styling(full_width = TRUE)
    env <- attr(out, "kable_meta")$tabular

    # begin/end use the same environment and it is a full-width one
    expect_match(out, paste0("\\\\begin\\{", env, "\\}"))
    expect_match(out, paste0("\\\\end\\{", env, "\\}"))
    # the table spreads to \linewidth using X columns
    expect_match(out, "linewidth")
    expect_match(out, "X")
    # one X column per data column
    expect_equal(
        lengths(regmatches(out, gregexpr("X", out))), 2
    )

    out_long <- kbl(df, format = "latex", longtable = TRUE) |>
        kable_styling(full_width = TRUE)
    env_long <- attr(out_long, "kable_meta")$tabular
    expect_match(out_long, paste0("\\\\begin\\{", env_long, "\\}"))
    expect_match(out_long, paste0("\\\\end\\{", env_long, "\\}"))
    expect_match(out_long, "linewidth")
})

test_that("LaTeX full_width: explicit latex_table_env overrides", {
    df <- data.frame(a = 1:4, b = 4:7)

    # Users may still explicitly request any environment, including the
    # legacy tabu, via latex_table_env. kableExtra performs the
    # substitution but no longer vouches for the package being loaded.
    out <- kbl(df, format = "latex") |>
        kable_styling(latex_table_env = "tabu")
    expect_match(out, "\\\\begin\\{tabu\\}")
    expect_match(out, "\\\\end\\{tabu\\}")
})
