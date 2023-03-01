## SDG name ---------------------------------------------------------------------------
sdg_name <- paste0("SDG", seq(1, 17, 1))

## SDG colors -------------------------------------------------------------------------


### RGB
color_rgb <- data.frame(
  R = c(229, 221, 76, 197, 255, 38, 252, 162, 253, 221, 253, 191, 63, 10, 86, 0, 25),
  G = c(36, 166, 159, 25, 58, 189, 195, 25, 105, 19, 157, 139, 126, 141, 192, 104, 72),
  B = c(59, 58, 56, 45, 33, 226, 11, 66, 37, 103, 36, 46, 68, 217, 43, 157, 106)
)

### HEX
color_hex <- rgb(color_rgb, max = 255)
names(color_hex) <- sdg_name

#' Color scheme for the 17 SDGs
#'
#' @usage sdg_color(x)
#'
#' @param x A number, which indicates the SDG ID
#'
#' @return HTML color code of a specified SDG
#' @export
#'
#' @examples
#' sdg_color(1)
#' sdg_color(x = 1:17)
sdg_color <- function(x) {
  color <- color_hex[x]
  scales::show_col(color)
  return(color)
}
