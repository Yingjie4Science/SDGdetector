# load database
load('./R/SDG_keys.RData')
library(dplyr)

# identify 17 Sustainable Development Goals and associated targets in text
# input: a dataframe with a column of "chr" type data
# output: coded dataframe with columns: statement, matched_sdg, matched_target

# separate goals/targets by column (then counts)?

findSDGs <- function(df) {

  text_df <- df %>% dplyr::select(where(is.character)) # locate the column of text
  colnames(text_df) <- "statement"
  coded_df <- text_df %>% dplyr::mutate(matched_sdg = '', matched_target = '')
                                        #sdg_count = '', target_count = '' # to add later

  for (i in 1:nrow(SDG_keys)) {
    sdg_id <- SDG_keys$SDG_id[i]
    target_id <- SDG_keys$target_id[i]
    target_key <- SDG_keys$SDG_keywords[i]

    coded_df <- coded_df %>%
      dplyr::mutate(match = ifelse(grepl(pattern = target_key, x = statement, ignore.case = T, perl = T), 1, 0)) %>%
      dplyr::mutate(matched_sdg = ifelse(match > 0, paste0(matched_sdg, sdg_id, ', '), matched_sdg),
                    matched_target = ifelse(match > 0, paste0(matched_target, target_id, ', '), matched_target)) %>%
      as.data.frame()
  }
  coded_df <- subset(coded_df, select = -match)
  return(coded_df)
}

test <- data.frame(boo=c('To this end, we tailor our products and services to suit our customers specific needsfrom home construction, improvement, and renovation to agricultural, industrial, and marine/hydraulic applications',
                         'define and carry out a worldwide survey across Working with a diverse group of stakeholderslistening to their concerns and managing our relations in a proactive and fruitful wayis crucial to understanding our ecosystem and maximizing our positive impact in the places where we operate',
                         'We are further leading the UN Global compact in Mexico, encouraging more companies to partner to contribute and explore business opportunities while reducing negative impact, and creating shared value to society'))

coded_test <- findSDGs(test)

