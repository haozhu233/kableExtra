# TODO: use system.file() instead of here::here()
test_that("Rmarkdown  compilation", {
    tmp <- tempfile(fileext = ".Rmd")
    file.copy(here::here("inst/rmarkdown/test_column_spec.Rmd"), tmp)
    compile <- try(rmarkdown::render(tmp, quiet = TRUE), silent = TRUE)
    expect_false(inherits(compile, "try-error"))
})


test_that("Rmarkdown example from inst/", {
    df <- data.frame(a = 1:4, b = 4:7)
    expect_snapshot(
        kbl(df, format = "latex") |>
            column_spec(2,
                bold = TRUE,
                monospace = TRUE,
                underline = TRUE,
                italic = TRUE,
                color = "red",
                background = "#FFFF00",
                width = "3in",
                border_right = TRUE
            )
    )

    dt <- mtcars[1:5, 1:6]
    expect_snapshot(
        kbl(dt, format = "latex", booktabs = TRUE) %>%
            kable_styling(full_width = TRUE) %>%
            column_spec(1, width = "8cm")
    )
})

test_that("LaTeX image dimensions use a period decimal mark", {
    df <- data.frame(item = c("a", "b"), image = "")
    table <- kbl(df, format = "latex", booktabs = TRUE)
    images <- spec_image(rep("fixture.png", 2), width = 50, height = 60, res = 300)
    render_image <- function(decimal_mark) {
        previous <- options(OutDec = decimal_mark)
        on.exit(options(previous), add = TRUE)
        as.character(column_spec(table, 2, image = images))
    }

    comma <- render_image(",")
    expect_identical(comma, render_image("."))
    expect_match(comma, "\\includegraphics[width=0.17in, height=0.2in]{fixture.png}",
                 fixed = TRUE)
})
