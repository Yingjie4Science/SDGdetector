
#' Detect country or region names in text for further mapping
#'
#' @description Detect country or region names in text for further mapping.
#'
#' @usage detect_region(x, col)
#'
#' @param x   Data frame or a string
#' @param col Column name for text to be assessed
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select
#'
#' @examples
#' x <- c("This paper explores the method and results from an independent
#' evidence based assessment of Australia's progress towards the SDGs",
#' "Last year alone, the United States experienced 14 separate billion-dollar
#'  disasters related to climate change")
#' col <- data.frame(x)
#' regions <- detect_region(x, col)
#'
#' @return Returns the tool text outputs.
#' @export



detect_region <- function(x, col) {

  # data(country_region_names, "country_region_names")

  ## --> input = a string ================================================================
  if (is.data.frame(x) == FALSE) {

    region <- ''
    ## loop and detect each country/region name
    for (i in 1:nrow(country_region_names)){
      region_i <- country_region_names$name[i]  ## get the region name

      ## if the number of character of a region name is less than 4, we need to add word boundary to the word
      region_i_enhanced <- ifelse(nchar(region_i) < 4,
                                  paste0('\\b', region_i, '\\b'),
                                  region_i)

      ## if the number of character of a region name is less than 4, case_sensitive = TRUE
      case_sensitive    <- ifelse(nchar(region_i) < 4,
                                  0,
                                  1)

      region <- ifelse(grepl(pattern = region_i_enhanced,
                            x = x,   ## using column names as function arguments, see https://stackoverflow.com/questions/48062213/dplyr-using-column-names-as-function-arguments
                            ignore.case = case_sensitive, perl = T),
                      paste0(region, ',', region_i),  ## If detected, add the region name to the cell
                      paste0(region, ''))

    }
    region <- gsub("^,*|(?<=,),|,*$", "", region, perl=T)
    ## - ref: https://stackoverflow.com/questions/23274035/removing-multiple-commas-and-trailing-commas-using-gsub




  ## --> input = a dataframe =============================================================
  } else {
    data <- x
    ## add a "region" column to the dataframe
    data$region <- ''

    ## loop and detect each country/region name
    for (i in 1:nrow(country_region_names)){
      region_i <- country_region_names$name[i]  ## get the region name

      ## if the number of character of a region name is less than 4, we need to add word boundary to the word
      region_i_enhanced <- ifelse(nchar(region_i) < 4,
                                  paste0('\\b', region_i, '\\b'),
                                  region_i)

      ## if the number of character of a region name is less than 4, case_sensitive = TRUE
      case_sensitive    <- ifelse(nchar(region_i) < 4,
                                  0,
                                  1)

      # print(region_i)
      # print(region_i_enhanced)
      # print(case_sensitive)

      data <- data %>%
        as.data.frame() %>%
        ## at the sentence level - detect if a subject country or region is mentioned ----------------
        dplyr::mutate(
          region = ifelse(
            grepl(
              pattern = region_i_enhanced,
              ## using column names as function arguments, see https://stackoverflow.com/questions/48062213/dplyr-using-column-names-as-function-arguments
              # x = !!sym(col),
              x = {{col}},
              # x = as.character({{col}}),
              ignore.case = case_sensitive,
              perl = T),
            paste0(region, ',', region_i),  ## If detected, add the region name to the cell
            paste0(region, '')))  %>%       ## If not, add nothing
        as.data.frame()
    }
    region <- data %>%
      dplyr::mutate(region = gsub("^,*|(?<=,),|,*$", "", region, perl=T))
}





  return(region)

}


### test
# x = 'China and USA devoted the largest efforts on solar energy'
# detect_region(x)
#
# x = data.frame(txt_col = c(
#   'China and USA devoted the largest efforts on solar energy',
#   'Congo needs to improve SDG 1 and 2'
#   ))
# detect_region(x, col = txt_col)
