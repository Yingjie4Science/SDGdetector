#' Identify SDGs in text
#'
#' @description
#' The `SDGdetector` function identify 17 Sustainable Development Goals and
#' associated 169 targets in text.
#'
#' @details
#' In 2015, leaders worldwide adopted 17 Sustainable Development Goals (SDGs) with 169
#' targets to be achieved by 2030 (https://sdgs.un.org). The framework of SDGs serves
#' as a blueprint for shared prosperity for both people and the earth. `SDGdetector`
#' identifies both direct and indirect expressions of SDGs and associated targets in
#' chunks of text. It takes a data frame with a specified column of text to process as
#' inputs and outputs a data frame with original columns plus matched SDGs and targets.
#'
#' @name SDGdetector
#'
#' @param df Data frame
#' @param col Column name for text to be assessed
#'
#' @return
#' Data frame with the same information as `df` and 18 extra columns: 17 columns marking
#' the number of occurrence of each of the 17 SDGs and one column `match_detail` listing
#' details of specific targets.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select relocate last_col
#' @importFrom stringr str_count
#'
#' @export
#'
#' @examples
#' my_text <- data.frame(my_col=c('our goal is to end poverty globally', 'this product
#' contributes to slowing down climate change'))
#' SDGdetector(my_text, my_col)


# load search terms of SDG targets
load('data/SDG_keys.RData')

SDGdetector <- function(df, col) {

  # initialize a column to record matched targets
  match_detail <- ''
  coded_df <- df %>% dplyr::mutate(match_detail=match_detail)

  # loop over all patterns in database and record matches
  for (i in 1:nrow(SDG_keys)) {

    key <- SDG_keys$SDG_keywords[i]
    id <- SDG_keys$SDG_id[i]

    coded_df <- coded_df %>%
      dplyr::mutate(match = ifelse(grepl(pattern = key, x = as.character({{col}}), ignore.case = T, perl = T), 1, 0)) %>%
      dplyr::mutate(match_detail = ifelse(match > 0, paste0(match_detail, id, ', '), match_detail)) %>%
      as.data.frame()
  }

  # record the number of occurrence of each of the 17 SDGs according to matched targets
  coded_df$SDG1 <- str_count(coded_df$match_detail, pattern = "SDG1_")
  coded_df$SDG2 <- str_count(coded_df$match_detail, pattern = "SDG2_")
  coded_df$SDG3 <- str_count(coded_df$match_detail, pattern = "SDG3_")
  coded_df$SDG4 <- str_count(coded_df$match_detail, pattern = "SDG4_")
  coded_df$SDG5 <- str_count(coded_df$match_detail, pattern = "SDG5_")
  coded_df$SDG6 <- str_count(coded_df$match_detail, pattern = "SDG6_")
  coded_df$SDG7 <- str_count(coded_df$match_detail, pattern = "SDG7_")
  coded_df$SDG8 <- str_count(coded_df$match_detail, pattern = "SDG8_")
  coded_df$SDG9 <- str_count(coded_df$match_detail, pattern = "SDG9_")
  coded_df$SDG10 <- str_count(coded_df$match_detail, pattern = "SDG10_")
  coded_df$SDG11 <- str_count(coded_df$match_detail, pattern = "SDG11_")
  coded_df$SDG12 <- str_count(coded_df$match_detail, pattern = "SDG12_")
  coded_df$SDG13 <- str_count(coded_df$match_detail, pattern = "SDG13_")
  coded_df$SDG14 <- str_count(coded_df$match_detail, pattern = "SDG14_")
  coded_df$SDG15 <- str_count(coded_df$match_detail, pattern = "SDG15_")
  coded_df$SDG16 <- str_count(coded_df$match_detail, pattern = "SDG16_")
  coded_df$SDG17 <- str_count(coded_df$match_detail, pattern = "SDG17_")

  # organize data frame for output
  coded_df <- coded_df %>% dplyr::select(-match) %>% relocate(match_detail, .after=last_col())

  return(coded_df)

}
