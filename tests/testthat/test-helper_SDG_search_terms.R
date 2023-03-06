test_that("test function - func_AND_vector()", {
  words <- c('apple', 'bean', 'food')
  output <- func_AND_vector(v= words)
  expect_length(output, 1)
  expect_equal(
    output,
    expected = "(?=.*(?:apple))(?=.*(?:bean))(?=.*(?:food))"
  )
})


test_that("test function - func_AND_plus()", {
  words <- c('apple', 'bean', 'food')
  output <- func_AND_plus(v= words)
  expect_length(output, 1)
  expect_equal(
    output,
    expected = "^(?=.*(?:apple))(?=.*(?:bean))(?=.*(?:food)).+"
  )
})


test_that("test function - func_OR_vector()", {
  words <- c('apple', 'bean', 'food')
  output <- func_OR_vector(v= words)
  expect_length(output, 1)
  expect_equal(
    output,
    expected = "(apple|bean|food)"
  )
})



test_that("test function - func_to_exclude_terms()", {
  exclude <- "Access Bank"
  SDG_xx = c(
    "access to|inclusi\\S*",
    "financial service.?|financial institution|\\bbanks|\\banking"
  )
  SDG_xx <- func_AND_plus(SDG_xx)
  output <- func_to_exclude_terms(which_sdg_term  = SDG_xx, terms_to_exclude = exclude)
  expect_length(output, 1)
  expect_equal(
    output,
    expected = "^(?!.*(?:Access Bank))(?=.*(?:access to|inclusi\\S*))(?=.*(?:financial service.?|financial institution|\\bbanks|\\banking)).+"
  )
})




test_that("test function - lookaround_nearby_n()", {
  con1 <- c('apple', 'bean', 'food')
  con2 <- c('big', 'delicious')

  output <- lookaround_nearby_n(word_ls1 = con1, word_ls2 = con2, n = 2, exclude = "", third_AND_string = "")
  expect_length(output, 3)
  expect_equal(
    output[1],
    expected = "(((?:apple)\\s(?:\\w+\\s){0,2}(?=(?:big)))|((?:big)\\s(?:\\w+\\s){0,2}(?=(?:apple))))"
  )


  output2 <- lookaround_nearby_n(word_ls1 = con1, word_ls2 = con2, n = 2, exclude = "", third_AND_string = "sustainable")
  expect_length(output2, 3)
  expect_equal(
    output2[1],
    expected = "^(?=.*(?:sustainable)).+(((?:apple)\\s(?:\\w+\\s){0,2}(?=(?:big)))|((?:big)\\s(?:\\w+\\s){0,2}(?=(?:apple))))"
  )
})


