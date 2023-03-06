test_that("geom_bar works", {
  data("sdgstat")
  p <- plot_sdg_bar(sdgstat, sdg = "SDG", value = "Value")
  x <- ggplot2::layer_data(p)

  expect_false(x$flipped_aes[1])
})
