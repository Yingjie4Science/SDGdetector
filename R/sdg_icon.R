# dir  <- dirname(rstudioapi::getSourceEditorContext()$path); dir
# path <- paste0(dirname(dir), "/inst/SDG-Icons-2019_WEB")
# image_pngs <- sort(sample(dir(path, full.names = TRUE), 18))

#' Icons for SDGs
#' @description
#' The `sdg_icon`  function provides the specific icon for each SDG
#'
#'
#' @param x Numeric code for each SDG, ranging from 1 to 17
#' @param res Resolution of SDG icon. Default: `res = 200` indicates
#'    resizing proportionally to 200px
#'
#' @importFrom magick image_scale
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' sdg_icon(x = 17, res = 300)
#'
sdg_icon <- function(x, res = 200) {
  ## --- approach 1
  # load('./data/sdg_icons.rda')
  # icon <- sdg_icons[i] %>% magick::image_scale(., geometry = res)

  ## --- approach 2
  # icon <- magick::image_read(path = image_pngs[x]) %>%
  #   magick::image_scale(., geometry = res)

  ## --- approach 3
  # png <-
  #   system.file(paste0("E-WEB-Goal-",  stringr::str_pad(x, 2, pad = "0"), "png"),
  #               package = "SDGdetector")
  # icon <-
  #   magick::image_read(path = png) %>% magick::image_scale(., geometry = res)

  ## --- approach 4
  image_pngs <-
    list.files(
      system.file("extdata", package = "SDGdetector"),
      pattern = "^E-WEB-Goal",
      full.names = TRUE
    )
  icon <- magick::image_read(path = image_pngs[x])
  icon <- magick::image_scale(image = icon, geometry = res)

  return(icon)
}

## test
sdg_icon(x = 17, res = 300)
