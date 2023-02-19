#' SDG Map Plot
#' @description SDG map plot
#'
#' @usage plot_sdg_map(data, sdg = sdg, value = value,
#'                     country = country, by_sdg = TRUE)
#'
#' @param data  Data frame as the input
#' @param value The value, e.g., number of SDGs, to be show in the thematic map
#' @param sdg   Vector with SDG code to be visualized.
#' @param country Country that are associated with the SDGs.
#' @param by_sdg If mapping by SDG, TRUE or FALSE.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select left_join group_by
#' @importFrom ggplot2 ggplot geom_sf scale_fill_distiller theme_bw element_blank element_rect unit aes guides guide_legend
#'             scale_x_continuous scale_y_continuous facet_wrap
#' @importFrom tidyr pivot_wider pivot_longer starts_with
#' @importFrom rnaturalearth ne_countries
#'
#' @examples
#' data("sdgstat")
#' plot_sdg_map(sdgstat, sdg = "SDG", value = "Value",
#'              country = "Country", by_sdg = FALSE)
#'
#' @return Returns the tool text outputs.
#' @export
#'
plot_sdg_map <- function(data, sdg = "sdg", value = "value",
                         country = "country", by_sdg = TRUE) {

  k <- which(names(data) %in% c(
    deparse(substitute(SDG)),
    deparse(substitute(Value)),
    deparse(substitute(Country))
    ))
  names(data)[k] <- c("sdg", "value", "country")

  sdg_name <- paste0("SDG", seq(1, 17, 1))

  data$sdg <- factor(data$sdg, levels = sdg_name)

  world <- ne_countries(scale = "small", returnclass = "sf")


  if(by_sdg == TRUE) {
    d1 <- data %>%
      group_by(country, sdg) %>%
      dplyr::summarise_at(c("value"), sum, na.rm = TRUE) %>%
      as.data.frame()

    wd <- merge(world, d1, by.x = "iso_a3", by.y = "country")

    p1 <- ggplot(wd) +
      geom_sf(data = world) +
      geom_sf(aes(fill = value)) +
      scale_fill_distiller(palette = 'YlGnBu', direction = 1, na.value = "gray80") +
      facet_wrap(~sdg) +
      theme_bw()
  } else {
    d1 <- data %>%
      group_by(country) %>%
      dplyr::summarise_at(c("value"), sum, na.rm = TRUE) %>%
      as.data.frame()

    wd <- merge(world, d1, by.x = "iso_a3", by.y = "country")

    p1 <- ggplot(wd) +
      geom_sf(data = world) +
      geom_sf(aes(fill = value)) +
      scale_fill_distiller(palette = 'YlGnBu', direction = 1, na.value = "gray80") +
      theme_bw()

  }



  return(p1)

}
