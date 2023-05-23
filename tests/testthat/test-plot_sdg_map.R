test_that("test if geom_sf works", {
  data("sdgstat")
  p <- plot_sdg_map(
    data = sdgstat,
    sdg = "SDG",
    value = "Value",
    country = "Country",
    by_sdg = FALSE
  )

  expect_identical(p$layers[[1]]$show.legend, NA)
  expect_identical(p$layers[[1]]$computed_geom_params$legend, NULL)

  # Perform minimal tests
  expect_error(regexp = NA, p)
})

test_that("Verify the input data for mapping", {
  data("sdgstat")
  df <- sdgstat[1:3]

  # expect_message(
  #   plot_sdg_map(
  #     data = df,
  #     sdg = "SDG",
  #     value = "Value",
  #     country = "Country",
  #     by_sdg = F
  #   ),
  #   "The input data must contain a minimum of three columns, including SDG name, numeric value, and geographic location."
  # )


  expect_error(
      plot_sdg_map(
        data = df,
        sdg = "SDG",
        value = "Value",
        country = "Country",
        by_sdg = F),
    "Data object must include columns [Country].", fixed=TRUE)

})


test_that("test plot mapping by sdgs", {
  data("sdgstat")
  p <- plot_sdg_map(
    data = sdgstat,
    sdg = "SDG",
    value = "Value",
    country = "Country",
    by_sdg = T
  )

  expect_error(regexp = NA, p)
})
