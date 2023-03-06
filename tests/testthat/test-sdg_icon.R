test_that("Return the icon information of a specified SDG", {
  output <- sdg_icon(x = 17, res = 300)

  output_check <- magick::image_info(output)

  expect_equal(
    output_check$format,
    expected = "PNG"
  )

  expect_equal(
    output_check$width,
    expected = 300
  )
})
