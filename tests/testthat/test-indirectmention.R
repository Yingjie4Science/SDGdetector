context("detect indirect expressions of SDGs and targets in text")

test_that("return data frames of correct size", {
  df1 <- data.frame(col=c('no poverty'))
  df2 <- data.frame(col=c('no poverty', 'zero hunger'))
  expect_true(ncol(findSDGs(df1, col)) == 19)
  expect_true(nrow(findSDGs(df1, col)) == 1)
  expect_true(ncol(findSDGs(df2, col)) == 19)
  expect_true(nrow(findSDGs(df2, col)) == 2)
})

test_that("Detect indirect mentions of SDGs and associated targets", {
  df1 <- data.frame(col=c('no poverty'))
  df2 <- data.frame(col=c('end hunger'))
  df3 <- data.frame(col=c('By 2030 eradicate extreme poverty for all people everywhere'))
  expect_equal(findSDGs(df1, col)$match_detail, 'SDG1_general, SDG1_1, SDG1_2, SDG3_3, ')
  expect_equal(findSDGs(df2, col)$match_detail, 'SDG2_1, SDG3_3, ')
  expect_equal(findSDGs(df3, col)$match_detail, 'SDG1_1, SDG1_2, SDG3_3, ')
})
