#' Plot SDG on the map
#'
#'
#' @param data  Data frame as the input
#' @param value The value, e.g., number of SDGs, to be show in the thematic map
#' @param sdg   Vector with SDG code to be visualized.
#' @param country Country that are associated with the SDGs.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select left_join group_by
#' @importFrom ggplot element_blank element_rect unit
#' @importFrom ggpubr
#' @importFrom RColorBrewer
#'
#' @return Returns the tool text outputs.
#' @export


### visualization parameters
# myPalette <- RColorBrewer::colorRampPalette((brewer.pal(8, "YlGnBu")));

font      <- 'sans'
font_size <- 10
theme_map <-
  ggpubr::theme_transparent()+
  ggplot2::theme(
    axis.title = ggplot2::element_blank(),
    axis.text  = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.border     = ggplot2::element_blank(),
    panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
    plot.background  = ggplot2::element_rect(fill = "transparent", colour = NA),
    legend.key.size  = ggplot2::unit(0.5, "cm"),
    legend.key       = ggplot2::element_rect(fill = NA, colour = NA, size = 0.25),
    legend.background= ggplot2::element_rect(fill = "transparent", colour = NA),
    legend.box.background = ggplot2::element_rect(fill = "transparent", colour = NA),
    text             =ggplot2::element_text(family=font, size=font_size))


### load global map shape files
load(file = paste0('./data/', 'shp_grp.RData'))      ## country income group; `grp`, `shp`

sdg_name <- paste0("SDG", seq(1, 17, 1))


### country names and code
githubURL <- ("https://raw.githubusercontent.com/vincentarelbundock/countrycode/main/data/codelist.rda")
githubURL <- ("https://raw.githubusercontent.com/vincentarelbundock/countrycode/main/data/codelist_panel.rda")
download.file(url = githubURL, destfile = "codelist_panel.rda", method="curl")
load(file = "codelist_panel.rda")
country_iso3c_list <- unique(codelist_panel$iso3c)





### plot bar function ================================================================== #

plot_sdg_bar <- function(
    data,
    sdg = NULL,
    value) {

  ## check if columns present
  required_columns <- c("sdg","value")
  if(any(!required_columns %in% names(data))){
    missing <- required_columns[!required_columns %in% names(data)]
    stop(paste0("Data object must include columns [",paste0(missing, collapse=", "),"]."))
  }

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

  ## better to sort sdg names in the order
  data$sdg = factor(data$sdg, levels = sdg_name)

  ## aggregate by SDGs
  data <- data %>%
    group_by(sdg) %>%
    dplyr::summarise_at(c("value"), sum, na.rm = TRUE)

  ## plot
  plot <- data %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = sdg, y = value, fill = sdg)) +
    ggplot2::geom_col(show.legend = F) +
    ggplot2::scale_fill_manual(values = sdg_color(x = 1:17)) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      axis.title.x = ggplot2::element_blank())
  return(plot)
}





### plot map function ================================================================== #
plot_sdg_map <- function(data, value, by_sdg = TRUE) {

  ## check if columns present
  required_columns <- c("sdg","value", "country")
  if(any(!required_columns %in% names(data))){
    missing <- required_columns[!required_columns %in% names(data)]
    stop(paste0("Data object must include columns [",paste0(missing, collapse=", "),"]."))
  }


  ## check country names - to be in the iso_3c format
  country_name_check <- unique(data$country)
  if(any(!country_name_check %in% country_iso3c_list)){
    message(paste0("country names must be in the format of ",  "iso_3c.",
                   " For example, 'the United States' should be converted to 'USA'"))
  }

  ## better to sort sdg names in the order
  data$sdg = factor(data$sdg, levels = sdg_name)

  ## format data and add shapefile information
  if(by_sdg == TRUE) {

    data_sf <- data %>%
      group_by(country, sdg) %>%
      dplyr::summarise_at(c("value"), sum, na.rm = TRUE) %>%
      as.data.frame() %>%
      tidyr::pivot_wider(names_from = sdg, values_from = value) %>%
      merge(
        x = shp, y= .,
        by.x = 'iso_a3',
        by.y = 'country',
        all.x = T) %>%
      tidyr::pivot_longer(cols = starts_with("SDG"),
                          names_to = 'sdg', values_to = 'value') %>%
      dplyr::mutate(sdg = factor(sdg, levels = sdg_name))

    ### plot facet by sdg
    plot <- data_sf %>%
      ggplot2::ggplot(data = .) +
      ggplot2::geom_sf(
        aes(fill = value),
        color='gray80',
        size = 0.1) +
      ggplot2::scale_fill_distiller(palette = 'YlGnBu', direction = 1, na.value = "gray80") +
      ggplot2::theme_bw() +
      guides(fill = guide_legend(
        label.hjust = 0,
        label = T,
        # direction = 'horizontal',
        # nrow = 1,
        reverse = F,
        title = 'SDG')) +
      theme_map +
      ggplot2::theme(legend.position = c(0.8, 0.08)) +
      facet_wrap(~sdg)



    } else {

      data_sf <- data %>%
        group_by(country) %>%
        dplyr::summarise_at(c("value"), sum, na.rm = TRUE) %>%
        as.data.frame() %>%
        dplyr::left_join(
          x = shp, y= .,
          by = c('iso_a3' = 'country')
          )

      ### plot facet by sdg
      plot <- data_sf %>%
        ggplot2::ggplot(data = .) +
        ggplot2::geom_sf(
          aes(fill = value),
          color='gray80',
          size = 0.1) +
        ggplot2::scale_fill_distiller(palette = 'YlGnBu', direction = 1, na.value = "gray80") +
        ggplot2::theme_bw() +
        guides(fill = guide_legend(
          label.hjust = 0,
          label = T,
          # direction = 'horizontal',
          # nrow = 1,
          reverse = T,
          title = 'SDG')) +
        scale_y_continuous(expand = c(0, 0)) + ## reduce the space between plot and border
        scale_x_continuous(expand = c(0, 0)) + ## reduce the space between plot and border
        labs(
          title = NULL,
          subtitle = NULL,
          caption = "Gray color denotes missing values"
        ) +
        theme_map +
        ggplot2::theme(
          legend.position = c(0.1, 0.35),
          plot.margin = margin(0,0,0,0,"pt"),
          plot.caption = ggplot2::element_text(
            color = "gray30", face = "italic",
            margin = margin(t = -15)))

    }

  return(plot)

}

### test
# df <- readr::read_csv('./data/data_example.csv')
# plot_sdg_bar(data = df, value = value)
# ggsave(filename = './docs/images/example_plots/plot_sdg_bar_example.png',
#        plot = last_plot(),
#        width = 7, height = 5, units = 'in')
#
# # plot_sdg_map(data = df, value = value, by_sdg = T)
# plot_sdg_map(data = df, value = value, by_sdg = F)
# ggsave(filename = './docs/images/example_plots/plot_sdg_map_example.png',
#        plot = last_plot(),
#        width = 7, height = 4, units = 'in'
#        )
