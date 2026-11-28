test_that("HTML: insert a single inline pointrange in a one-row table", {
    plot <- spec_pointrange(1.2, 0.2, 1.5, file_type = svglite::svglite, dir = tempdir())
    table <- kbl(data.frame(item = "a", plot = ""), format = "html")
    output <- column_spec(table, 2, image = plot)
    expect_identical(as.character(output),
                     as.character(column_spec(table, 2, image = list(plot))))
    expect_match(output, "<svg", fixed = TRUE)
})

test_that("LaTeX: keep dimensions for a single inline image", {
    image <- spec_image("fixture.png", width = 100, height = 200, res = 100)
    table <- kbl(data.frame(item = "a", image = ""), format = "latex")
    output <- column_spec(table, 2, image = image)
    expect_identical(as.character(output),
                     as.character(column_spec(table, 2, image = list(image))))
    expect_match(output, "\\includegraphics[width=1in, height=2in]{fixture.png}",
                 fixed = TRUE)
})


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
