test_that("Issue #887: multiple escapes", {
  expect_snapshot(
    kbl(x = head(mtcars), format = "latex") %>%
      pack_rows(group_label = "Group $\\Delta = \\text{A}^1$",
                start_row = 2, end_row = 5, escape = FALSE)
  )
})

test_that("Issue #876: complex alignment", {
  expect_snapshot(
    kbl(x = mtcars[1:2,1:2],
        format = "latex",
        align = rep("p{2cm}",2)) %>%
      kable_styling(latex_options = "scale_down")
  )
})


test_that("Issue #861:  pack_rows with tabularx", {
  expect_snapshot(
    kbl(mtcars, format="latex", tabular = "tabularx",
        valign="{\\textwidth}") %>%
      kableExtra::pack_rows("XXX", 1,2)

  )
}
)

test_that("Issue #836:  latex allowed in add_header_above", {
  expect_snapshot(
    kbl(mtcars[1:2, 1:2], col.names=NULL,
             format = "latex") |>
    add_header_above("\\textbf{HEADER}", escape = FALSE)
  )
}
)

test_that("Issue #812:  table without header works in collapse_rows", {
  tab <- kbl(mtcars[1:3, 1:4], col.names = NULL,
             format = "html", booktabs = TRUE) |>
    kable_styling(full_width=TRUE) |>
    collapse_rows(columns=2:3)
  expect_s3_class(tab, "knitr_kable")
})

test_that("Issue #806: custom rule widths", {
  expect_snapshot(
    kbl(mtcars[1:3, 1:4],
        caption="kable vary line thickness",
        booktabs = TRUE,
        toprule = "\\toprule[4pt]",
        midrule = "\\midrule[3pt]",
        bottomrule = "\\bottomrule[5pt]",
        linesep = "\\midrule[2pt]") |>
      kable_styling(repeat_header_text = TRUE) |>
      add_header_above(c("", "Group 1" = 2, "Group 2" = 2)) |>
      add_footnote("The footnote") |>
      footnote("Another footnote")
  )
})

test_that("Issue #796", {
    expect_snapshot(
        kbl(mtcars[1:3, 1:4], caption = "Demo table", booktabs = TRUE, format = "latex") |>
            kable_styling(latex_options = c("striped", "hold_position"))
    )
})


# Issue #658: column_spec() fails at single-row headerless tables in latex format.
dat <- data.frame(x = 1, y = 1)
dat <- setNames(dat, NULL)
tab <- kbl(dat, format = "latex") %>%
  column_spec(2, latex_column_spec = "l")
expect_s3_class(tab, "knitr_kable")


# Issue #613: Errors with single row tables using latex_options = "striped" and col.names = NULL
a <- data.frame(b = "Hello World", c = "Goodbye")
tab <- kbl(a, col.names = NULL, format = "latex") %>%
    column_spec(1, width = "16cm")
expect_s3_class(tab, "knitr_kable")

tab <- kbl(a, col.names = NULL, format = "latex") %>%
    kable_styling(latex_options = c("striped"), position = "left")
expect_s3_class(tab, "knitr_kable")



# Issue #534
BelQ <- structure(list(Source = "Kalender 2019", `Sourse of Questions` = "The survey instruments were part of a larger survey that included other motivational constructs. The development and validation of these surveys is reported in prior work [18–22,59]. ", Results = "PCA analysis reveals that for both genders, sense of belonging and identity were not separate from the Expectancy Value Theory constructs. Instead, sense of belonging was closely tied to self-efficacy, which was therefore labeled as “self-efficacy or belonging” component or factor. ", Questions = "Sense of Belonging or Self-efficacy (not separated in a paper):<br>1. Sometimes I worry that I do not belong in this physics class<br>2. I feel like I can be myself in this class<br>3. I am able to help my classmates with physics in the laboratory or in recitation<br>4. I understand concepts I have studied in physics<br>5. If I wanted to, I could be good at physics research<br>6. ", Reference = "Kalender, Z. Y., Marshman, E., Schunn, C. D., Nokes-Malach, T. J., & Singh, C. (2019). Gendered patterns in the construction of physics identity from motivational factors. Physical Review Physics Education Research, 15(2), [020119](https://journals.aps.org/prper/abstract/10.1103/PhysRevPhysEducRes.15.020119)"), row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame"))

# mtcars[1:4, 1:4] |> kbl() |> column_spec(1, italic = TRUE)
tab <- BelQ |>
    kbl(escape = FALSE) |>
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"), font_size = 12, full_width = TRUE) |>
    column_spec(1, width = "40em", include_thead = TRUE) |>
    scroll_box(width = "100%", height = "500px")
expect_s3_class(tab, "kableExtra")

tab <- BelQ |>
    kbl(escape = FALSE) |>
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"), font_size = 12, full_width = TRUE) |>
    column_spec(1, width = "40em", include_thead = FALSE) |>
    scroll_box(width = "100%", height = "500px")
expect_s3_class(tab, "kableExtra")

## This breaks because scroll_box breaks `kable_as_html()`
# tab <- BelQ |>
#     kbl(escape = FALSE) |>
#     kable_styling(bootstrap_options = c("striped", "hover", "condensed"), font_size = 12, full_width = TRUE) |>
#     scroll_box(width = "100%", height = "500px") |>
#     column_spec(1, width = "40em", include_thead = FALSE)
# expect_s3_class(tab, "kableExtra")


