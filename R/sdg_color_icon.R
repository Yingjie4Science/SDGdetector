

## This script provides
##  * SDG colors
##  * SDG icons
## Reference:
## UN Guidelines for the use of the SDG logo, including the colour wheel and 17 icons
## Link: https://www.un.org/sustainabledevelopment/wp-content/uploads/2019/01/SDG_Guidelines_AUG_2019_Final.pdf (updated in May 2020)

#'
#' @description
#' The `sdg_color` function provides the specific color code for each SDG
#' The `sdg_icon`  function provides the specific icon for each SDG
#'
#'
#' @param x Numeric code for each SDG, ranging from 1 to 17
#' @param res Resolution of SDG icon. Default: `res = 200` indicates resizing proportionally to 200px
#'
#' @importFrom pacman
#' @importFrom scales
#' @importFrom magick
#'
#' @examples
#' my_text <- data.frame(my_col=c('our goal is to end poverty globally', 'this product
#' contributes to slowing down climate change'))
#' SDGdetector(my_text, my_col)
#'

# if (!require("pacman")) install.packages("pacman"); library(pacman)
# pacman::p_load(scales, magick)
# library(scales)
# library(magick)


## SDG name ----------------------------------------------------------------------------------------
sdg_name <- paste0("SDG", seq(1, 17, 1))

## SDG colors ---------------------------------------------------------------------------------------


### RGB
color_rgb <-data.frame(R=c(229,221,76, 197,255,38, 252,162,253,221,253,191,63, 10, 86, 0,  25),
                       G=c(36, 166,159,25, 58, 189,195,25, 105,19, 157,139,126,141,192,104,72),
                       B=c(59, 58, 56, 45, 33, 226,11, 66, 37, 103,36, 46, 68, 217,43, 157,106))

### HEX
color_hex <- rgb(color_rgb, max=255)
names(color_hex) <- sdg_name
sdg_color <- function(x){
  color <- color_hex[x]
  scales::show_col(color)
  return(color)
}

## test
# sdg_color(1)
# sdg_color(x = 1:17)


### PMS


### CMYK


## SDG icons  ---------------------------------------------------------------------------------------

### list the icon files
image_png <- sort(sample(dir("docs/images/SDG-Icons-2019_WEB", full.names = TRUE), 18))

### the function
sdg_icon <- function(x, res = 200){
  icon <- magick::image_read(path = image_png[x]) %>% magick::image_scale(., geometry = res)
  return(icon)
}
# ## test
# sdg_icon(x = 17, res = 300)
