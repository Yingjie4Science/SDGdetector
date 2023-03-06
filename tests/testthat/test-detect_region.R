test_that("Detect country or region names in a string", {

  x = 'China and USA devoted the largest efforts on solar energy'

  output <- detect_region(x)

  expect_equal(
    output,
    expected = "China,USA"
  )

})


test_that("Detect country or region names in text from a dataframe", {

  x = data.frame(txt_col = c(
    'China and USA devoted the largest efforts on solar energy',
    'Congo needs to improve SDG 1 and 2'
  ))

  output <- detect_region(x, col = txt_col)

  expect_equal(
    output$region,
    expected = c("China,USA", "Congo")
  )

})



