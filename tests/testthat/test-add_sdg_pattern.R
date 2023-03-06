test_that("Add Customized Patterns for a SDG Target", {
  terms_new <- c("improve", "farmer", "income")

  output <- add_sdg_pattern(
    sdg_id = "SDG1_2",
    x = terms_new,
    operator = "AND"
  )

  output_test_data <- tail(output, 1)

  expect_equal(
    output_test_data$SDG_keywords,
    expected = "(?=.*(?:improve))(?=.*(?:farmer))(?=.*(?:income))"
  )

  expect_equal(
    output_test_data$match_tpye,
    expected = "user_defined"
  )
})


test_that("Add Customized Patterns for a SDG Target", {
  terms_new <- c("improve", "farmer", "income")

  output <- add_sdg_pattern(
    sdg_id = "SDG1_2",
    x = terms_new,
    operator = "OR"
  )

  output_test_data <- tail(output, 1)

  expect_equal(
    output_test_data$SDG_keywords,
    expected = "(improve|farmer|income)"
  )

  expect_equal(
    output_test_data$match_tpye,
    expected = "user_defined"
  )
})


test_that("Check if the input SDG name is correct", {
  terms_new <- c("improve", "farmer", "income")

  expect_error(
    add_sdg_pattern(
      sdg_id = "SDG1_12",
      x = terms_new,
      operator = "OR"
    )
  )
})

