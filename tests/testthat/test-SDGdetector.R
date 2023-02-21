test_that("Return data frames of correct size", {
  text <- 'our goal is to mitigate climate change, end poverty, and reduce inequality globally'
  df <- data.frame(col = c(
    'our goal is to end poverty globally',
    'this product contributes to slowing down climate change'))
  expect_true(ncol(SDGdetector(x = text)) == 3)
  expect_true(nrow(SDGdetector(x = text)) == 1)
  expect_true(ncol(SDGdetector(x = df, col = col)) == 3)
  expect_true(nrow(SDGdetector(x = df, col = col)) == 2)
})

test_that("Detect indirect mentions of SDGs and associated targets", {
  text <- 'our goal is to mitigate climate change globally'
  df <- data.frame(col = c(
    'our goal is to end poverty globally',
    'this product contributes to slowing down climate change'))
  expect_equal(SDGdetector(x = text)$sdgs, expected = "SDG13_2,SDG13_general")
  expect_equal(SDGdetector(x = df, col = col)$sdgs, expected = c("SDG13_2,SDG13_general", "SDG1_2"))
})
