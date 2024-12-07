test_that("add_header_above accepts p{3cm}", {
  expect_snapshot(
    kbl(
      mtcars[1:3, 1:3], 'latex'
    ) %>%
      add_header_above(c(' ',
                       'This is a very long header that will need to be wrapped' = 3),
                     align = c('l', 'p{3cm}')))
})
