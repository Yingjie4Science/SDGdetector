context("detect indirect expressions of SDGs and targets in text")

test_that("return data frames of correct size", {
  df1 <- data.frame(col=c('no poverty'))
  df2 <- data.frame(col=c('no poverty', 'zero hunger'))
  expect_true(ncol(SDGdetector(df1, col)) == 5)
  expect_true(nrow(SDGdetector(df1, col)) == 1)
  expect_true(ncol(SDGdetector(df2, col)) == 5)
  expect_true(nrow(SDGdetector(df2, col)) == 2)
})

test_that("Detect indirect mentions of SDGs and associated targets", {
  df1 <- data.frame(col=c('no poverty'))
  df2 <- data.frame(col=c('end hunger'))
  df3 <- data.frame(col=c('By 2030 eradicate extreme poverty for all people everywhere'))
  expect_equal(SDGdetector(df1, col)$sdgs, 'SDG1_general,SDG1_2')
  expect_equal(SDGdetector(df2, col)$sdgs, 'SDG2_1')
  expect_equal(SDGdetector(df3, col)$sdgs, 'SDG1_1,SDG1_2')
})
