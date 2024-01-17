test_that("add_indent can add to 1 row", {
  observed <- kable(mtcars[1:4, 1:3], "html") %>%
    add_indent(1) %>%
    as.character()
  expected <- "<table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   </th>\n   <th style=\"text-align:right;\"> mpg </th>\n   <th style=\"text-align:right;\"> cyl </th>\n   <th style=\"text-align:right;\"> disp </th>\n  </tr>\n </thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;padding-left: 2em;\" indentlevel=\"1\"> Mazda RX4 </td>\n   <td style=\"text-align:right;\"> 21.0 </td>\n   <td style=\"text-align:right;\"> 6 </td>\n   <td style=\"text-align:right;\"> 160 </td>\n  </tr>\n  <tr>\n   <td style=\"text-align:left;\"> Mazda RX4 Wag </td>\n   <td style=\"text-align:right;\"> 21.0 </td>\n   <td style=\"text-align:right;\"> 6 </td>\n   <td style=\"text-align:right;\"> 160 </td>\n  </tr>\n  <tr>\n   <td style=\"text-align:left;\"> Datsun 710 </td>\n   <td style=\"text-align:right;\"> 22.8 </td>\n   <td style=\"text-align:right;\"> 4 </td>\n   <td style=\"text-align:right;\"> 108 </td>\n  </tr>\n  <tr>\n   <td style=\"text-align:left;\"> Hornet 4 Drive </td>\n   <td style=\"text-align:right;\"> 21.4 </td>\n   <td style=\"text-align:right;\"> 6 </td>\n   <td style=\"text-align:right;\"> 258 </td>\n  </tr>\n</tbody>\n</table>"
  expect_equal(observed, expected)
})

test_that("add_indent can be added multiple times.", {
  observed <- kable(mtcars[1:4, 1:3], "html") %>%
    add_indent(1:3) %>%
    add_indent(1) %>%
    as.character()
  expected <- "<table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   </th>\n   <th style=\"text-align:right;\"> mpg </th>\n   <th style=\"text-align:right;\"> cyl </th>\n   <th style=\"text-align:right;\"> disp </th>\n  </tr>\n </thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;padding-left: 4em;\" indentlevel=\"2\"> Mazda RX4 </td>\n   <td style=\"text-align:right;\"> 21.0 </td>\n   <td style=\"text-align:right;\"> 6 </td>\n   <td style=\"text-align:right;\"> 160 </td>\n  </tr>\n  <tr>\n   <td style=\"text-align:left;padding-left: 2em;\" indentlevel=\"1\"> Mazda RX4 Wag </td>\n   <td style=\"text-align:right;\"> 21.0 </td>\n   <td style=\"text-align:right;\"> 6 </td>\n   <td style=\"text-align:right;\"> 160 </td>\n  </tr>\n  <tr>\n   <td style=\"text-align:left;padding-left: 2em;\" indentlevel=\"1\"> Datsun 710 </td>\n   <td style=\"text-align:right;\"> 22.8 </td>\n   <td style=\"text-align:right;\"> 4 </td>\n   <td style=\"text-align:right;\"> 108 </td>\n  </tr>\n  <tr>\n   <td style=\"text-align:left;\"> Hornet 4 Drive </td>\n   <td style=\"text-align:right;\"> 21.4 </td>\n   <td style=\"text-align:right;\"> 6 </td>\n   <td style=\"text-align:right;\"> 258 </td>\n  </tr>\n</tbody>\n</table>"
  expect_equal(observed, expected)
})

test_that("add_indent can add to an interior column.", {
  cars <- mtcars[1:4, 1:3]
  cars$cyl <- as.character(cars$cyl)
  observed <- kable(cars, "html") %>%
    add_indent(1:3, target_cols = 3) %>%
    as.character()
  expected <- "<table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   </th>\n   <th style=\"text-align:right;\"> mpg </th>\n   <th style=\"text-align:left;\"> cyl </th>\n   <th style=\"text-align:right;\"> disp </th>\n  </tr>\n </thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> Mazda RX4 </td>\n   <td style=\"text-align:right;\"> 21.0 </td>\n   <td style=\"text-align:left;padding-left: 2em;\" indentlevel=\"1\"> 6 </td>\n   <td style=\"text-align:right;\"> 160 </td>\n  </tr>\n  <tr>\n   <td style=\"text-align:left;\"> Mazda RX4 Wag </td>\n   <td style=\"text-align:right;\"> 21.0 </td>\n   <td style=\"text-align:left;padding-left: 2em;\" indentlevel=\"1\"> 6 </td>\n   <td style=\"text-align:right;\"> 160 </td>\n  </tr>\n  <tr>\n   <td style=\"text-align:left;\"> Datsun 710 </td>\n   <td style=\"text-align:right;\"> 22.8 </td>\n   <td style=\"text-align:left;padding-left: 2em;\" indentlevel=\"1\"> 4 </td>\n   <td style=\"text-align:right;\"> 108 </td>\n  </tr>\n  <tr>\n   <td style=\"text-align:left;\"> Hornet 4 Drive </td>\n   <td style=\"text-align:right;\"> 21.4 </td>\n   <td style=\"text-align:left;\"> 6 </td>\n   <td style=\"text-align:right;\"> 258 </td>\n  </tr>\n</tbody>\n</table>"
  expect_equal(observed, expected)
})

test_that("add_indent can add to an interior column multiple times.", {
  cars <- mtcars[1:4, 1:3]
  cars$cyl <- as.character(cars$cyl)
  observed <- kable(cars, "html") %>%
    add_indent(1:3, target_cols = 3) %>%
    add_indent(1, target_cols = 3) %>%
    as.character()
  expected <- "<table>\n <thead>\n  <tr>\n   <th style=\"text-align:left;\">   </th>\n   <th style=\"text-align:right;\"> mpg </th>\n   <th style=\"text-align:left;\"> cyl </th>\n   <th style=\"text-align:right;\"> disp </th>\n  </tr>\n </thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:left;\"> Mazda RX4 </td>\n   <td style=\"text-align:right;\"> 21.0 </td>\n   <td style=\"text-align:left;padding-left: 4em;\" indentlevel=\"2\"> 6 </td>\n   <td style=\"text-align:right;\"> 160 </td>\n  </tr>\n  <tr>\n   <td style=\"text-align:left;\"> Mazda RX4 Wag </td>\n   <td style=\"text-align:right;\"> 21.0 </td>\n   <td style=\"text-align:left;padding-left: 2em;\" indentlevel=\"1\"> 6 </td>\n   <td style=\"text-align:right;\"> 160 </td>\n  </tr>\n  <tr>\n   <td style=\"text-align:left;\"> Datsun 710 </td>\n   <td style=\"text-align:right;\"> 22.8 </td>\n   <td style=\"text-align:left;padding-left: 2em;\" indentlevel=\"1\"> 4 </td>\n   <td style=\"text-align:right;\"> 108 </td>\n  </tr>\n  <tr>\n   <td style=\"text-align:left;\"> Hornet 4 Drive </td>\n   <td style=\"text-align:right;\"> 21.4 </td>\n   <td style=\"text-align:left;\"> 6 </td>\n   <td style=\"text-align:right;\"> 258 </td>\n  </tr>\n</tbody>\n</table>"
  expect_equal(observed, expected)
})
