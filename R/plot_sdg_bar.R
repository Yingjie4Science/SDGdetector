#' SDG bar plot
#' @description SDG bar plot
#'
#' @usage plot_sdg_bar(data, sdg = sdg, value = value)
#'
#' @param data  Data frame as the input
#' @param value The value, e.g., number of SDGs, to be show in the thematic map
#' @param sdg   Vector with SDG code to be visualized.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select left_join group_by
#' @importFrom ggplot2 ggplot geom_col ggplot element_blank element_rect unit scale_fill_manual theme_bw theme
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom utils data
#' @importFrom grDevices rgb
#'
#' @examples
#' data("sdgstat")
#' plot_sdg_bar(sdgstat, sdg = "SDG", value = "Value")
#'
#' @return Returns the tool text outputs.
#' @export
#'
plot_sdg_bar <- function(data, sdg = "sdg", value = "value") {

  k <- which(names(data) %in% c(deparse(substitute(SDG)), deparse(substitute(Value))))
  names(data)[k] <- c("sdg", "value")

  sdg_name <- paste0("SDG", seq(1, 17, 1))

  data$sdg <- factor(data$sdg, levels = sdg_name)



  ## format data
  data <- data %>%
    dplyr::filter(sdg %in% sdg_name) %>%
    dplyr::mutate(
      ## remove extra spaces and punctuation from the text of SDG names
      sdg = gsub(' ', '', sdg),
      sdg = gsub('[[:punct:] ]+',' ', sdg))

  ## check values in the `sdg` column
  sdg_column_unique <- unique(data$sdg)
  if(any(!sdg_column_unique %in%sdg_name)){
    message(paste0("sdg names must be in the format of ",  sdg_name))
  }


  ## aggregate by SDGs
  data <- data %>%
    dplyr::group_by(sdg) %>%
    dplyr::summarise_at(c("value"), sum, na.rm = TRUE)

  color_rgb <-data.frame(R=c(229,221,76, 197,255,38, 252,162,253,221,253,191,63, 10, 86, 0,  25),
                         G=c(36, 166,159,25, 58, 189,195,25, 105,19, 157,139,126,141,192,104,72),
                         B=c(59, 58, 56, 45, 33, 226,11, 66, 37, 103,36, 46, 68, 217,43, 157,106))

  ### HEX
  color_hex <- rgb(color_rgb, maxColorValue = 255)
  names(color_hex) <- sdg_name
  sdg_color <- function(x){
    color <- color_hex[x]
    return(color)
  }

  ## plot
  p1 <- ggplot(data, aes(x = sdg, y = value, fill = sdg)) +
    geom_col(show.legend = F) +
    scale_fill_manual(values = sdg_color(x = 1:17)) +
    theme_bw() +
    theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      axis.title.x = ggplot2::element_blank())


  return(p1)
}

