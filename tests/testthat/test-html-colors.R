test_that("Issue #902: CSS alpha", {
  expect_equal(
    html_color_("#FF0000"),
    "rgba(255, 0, 0, 1)"
  )
  # alpha is divided by 255, will rarely be short/perfect decimals
  # without unnecessary rounding
  expect_match(
    html_color_("#FF000080"),
    "rgba\\(255, 0, 0, 0\\.5\\)"
  )
})
