test_that("Return the color for a specified SDG", {
  output <- sdg_color(x = 1)

  expect_equal(
    unname(output),
    expected = "#E5243B"
  )
})


test_that("Return the color for a list of specified SDGs", {
  output <- sdg_color(x = 1:17)

  expect_equal(
    unname(output),
    expected = c(
      "#E5243B", "#DDA63A", "#4C9F38", "#C5192D", "#FF3A21", "#26BDE2", "#FCC30B",
      "#A21942", "#FD6925", "#DD1367", "#FD9D24", "#BF8B2E", "#3F7E44", "#0A8DD9",
      "#56C02B", "#00689D", "#19486A"
    )
  )
})
