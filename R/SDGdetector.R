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

SDGdetector <- function(x, col) {

  ## first, to check the input is a string or a dataframe --------------------------------

  if (is.data.frame(x) == FALSE) {
    ## -> if not a dataframe
    # print('change/put the string into a dataframe')
    df <- data.frame(col = x) %>%
      dplyr::mutate(id = dplyr::row_number())

  } else {
    ##
    df <- x
  }


  ## -------------------------------------------------------------------------------------
  code <- df %>%
    dplyr::mutate(#match   = 0,
      sdgs    = '', ## for later use, to append data to this column
      n_total = 0,  ## count the accumulated mentions
      sdgs_n  = '') ## ## combine together SDG names and the number of mentions

  for (i in 1:nrow(SDG_keys)){                           ## loop each SDG indicators
    sdg_i_str <- SDG_keys$SDG_id[i] %>% as.character()   ## get the SDG id name
    sdg_i_obj <- SDG_keys$SDG_keywords[i]                ## get the corresponding SDG search term list

    print(sdg_i_str)
    # print(sdg_i_obj)

    code <-  code %>%
      as.data.frame() %>%

      ## at the sentence level - count once if goals/targets are mentioned ---------------
      dplyr::mutate(
        match = ifelse(
          grepl(pattern = sdg_i_obj, x = as.character({{col}}), ignore.case = T, perl = T), 1, 0))  %>% ## yes-1 or no-0 if they match
      dplyr::mutate(
          sdgs  = ifelse(match > 0, paste0(sdgs, ',', sdg_i_str), sdgs)) %>%
      dplyr::select(-match) %>% ## remove this column

      ## at the sentence level - count the times of all the mentions -------------------
      # dplyr::mutate(
      #   n       = str_count(string = as.character({{col}}), regex(pattern = sdg_i_obj, ignore_case = T)),
      #   n_total = n_total + n,
      #   sdgs_n  = ifelse(n > 0, paste0(sdgs_n, ',', sdg_i_str, '-', n), sdgs_n)
      # ) %>%
      # dplyr::select(-n) %>%   ## remove this column
      as.data.frame()
  }


  ### sort from most SDG hits to least (or, none)
  coded <- code %>% dplyr::arrange(desc(nchar(sdgs)), id)

  return(coded)


  ### by Meng Cai ------------------------------------------------------------------------
  # # initialize a column to record matched targets
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
  #
  # # record the number of occurrence of each of the 17 SDGs according to matched targets
  # coded_df$SDG1 <- str_count(coded_df$match_detail, pattern = "SDG1_")
  # coded_df$SDG2 <- str_count(coded_df$match_detail, pattern = "SDG2_")
  # coded_df$SDG3 <- str_count(coded_df$match_detail, pattern = "SDG3_")
  # coded_df$SDG4 <- str_count(coded_df$match_detail, pattern = "SDG4_")
  # coded_df$SDG5 <- str_count(coded_df$match_detail, pattern = "SDG5_")
  # coded_df$SDG6 <- str_count(coded_df$match_detail, pattern = "SDG6_")
  # coded_df$SDG7 <- str_count(coded_df$match_detail, pattern = "SDG7_")
  # coded_df$SDG8 <- str_count(coded_df$match_detail, pattern = "SDG8_")
  # coded_df$SDG9 <- str_count(coded_df$match_detail, pattern = "SDG9_")
  # coded_df$SDG10 <- str_count(coded_df$match_detail, pattern = "SDG10_")
  # coded_df$SDG11 <- str_count(coded_df$match_detail, pattern = "SDG11_")
  # coded_df$SDG12 <- str_count(coded_df$match_detail, pattern = "SDG12_")
  # coded_df$SDG13 <- str_count(coded_df$match_detail, pattern = "SDG13_")
  # coded_df$SDG14 <- str_count(coded_df$match_detail, pattern = "SDG14_")
  # coded_df$SDG15 <- str_count(coded_df$match_detail, pattern = "SDG15_")
  # coded_df$SDG16 <- str_count(coded_df$match_detail, pattern = "SDG16_")
  # coded_df$SDG17 <- str_count(coded_df$match_detail, pattern = "SDG17_")
  #
  # # organize data frame for output
  # coded_df <- coded_df %>% dplyr::select(-match) %>% relocate(match_detail, .after=last_col())
  #
  # return(coded_df)

}
