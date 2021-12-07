#' Identify SDGs in text
#'
#' @description
#' The `findSDGs` function identify 17 Sustainable Development Goals and
#' associated targets in text. It adds two columns, matched_sdg and matched_target,
#' to the column of text data.
#'
#' @details
#' to be added
#'
#' @param df A Data frame with a column of "chr" type data
#'
#' @return
#' A coded data frame with columns: statement, matched_sdg, matched_target
#'
#' @import dplyr
#'
#' @example
#' exp <- data.frame(boo=c('To this end, we tailor our products and services to suit
#' our customers specific needs from home construction, improvement, and renovation to
#' agricultural, industrial, and marine/hydraulic applications', 'define and carry out
#' a worldwide survey across Working with a diverse group of stakeholders listening to
#' their concerns and managing our relations in a proactive and fruitful way is crucial
#' to understanding our ecosystem and maximizing our positive impact in the places
#' where we operate'))
#' findSDGs(exp)
#'
#' @export
#'

# load database
load('./R/SDG_keys.RData')
library(dplyr)
library(stringr)

findSDGs <- function(df, col) {

  # set up columns to be coded
  coded_df <- df %>% dplyr::mutate(match_detail='')

  #
  for (i in 1:nrow(SDG_keys)) {
    key <- SDG_keys$SDG_keywords[i]
    id <- SDG_keys$SDG_id[i]

    coded_df <- coded_df %>%
      dplyr::mutate(match = ifelse(grepl(pattern = key, x = {{col}}, ignore.case = T, perl = T), 1, 0)) %>%
      dplyr::mutate(match_detail = ifelse(match > 0, paste0(match_detail, id, ', '), match_detail)) %>%
      as.data.frame()
  }

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

  coded_df <- coded_df %>% select(-match) %>% relocate(match_detail, .after=last_col())

  return(coded_df)
}

test <- data.frame(boo=c('end poverty',
                         'no hunger',
                         'To this end, we tailor our products and services to suit our customers specific needsfrom home construction, improvement, and renovation to agricultural, industrial, and marine/hydraulic applications',
                         'define and carry out a worldwide survey across Working with a diverse group of stakeholderslistening to their concerns and managing our relations in a proactive and fruitful wayis crucial to understanding our ecosystem and maximizing our positive impact in the places where we operate',
                         'We are further leading the UN Global compact in Mexico, encouraging more companies to partner to contribute and explore business opportunities while reducing negative impact, and creating shared value to society'))

coded_test <- findSDGs(test, boo)
