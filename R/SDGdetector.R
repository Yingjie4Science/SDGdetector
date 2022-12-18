#' Identify SDGs in text
#'
#' @description
#' Identify 17 Sustainable Development Goals and associated 169 targets in text.
#'
#' @usage SDGdetector(x, col)
#'
#' @details
#' In 2015, leaders worldwide adopted 17 Sustainable Development Goals (SDGs) with 169
#' targets to be achieved by 2030 (https://sdgs.un.org). The framework of SDGs serves
#' as a blueprint for shared prosperity for both people and the earth. `SDGdetector`
#' identifies both direct and indirect expressions of SDGs and associated targets in
#' chunks of text. It takes a data frame with a specified column of text to process as
#' inputs and outputs a data frame with original columns plus matched SDGs and targets.
#'
#'
#' @param x   Data frame or a string
#' @param col Column name for text to be assessed
#'
#' @return
#' Data frame with the same information as `df` and 18 extra columns: 17 columns marking
#' the number of occurrence of each of the 17 SDGs and one column `match_detail` listing
#' details of specific targets.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select relocate last_col desc
#' @importFrom stringr str_count str_length
#'
#' @export
#'
#' @examples
#' my_col=c('our goal is to end poverty globally', 'this product
#' contributes to slowing down climate change')
#' my_text <- data.frame(my_col)
#' SDGdetector(my_text, my_col)


SDGdetector <- function(x, col) {
  nchr <- sdgs <- id <- NULL

  # data(SDG_keys, "SDG_keys")

  ## first, to check the input is a string or a dataframe --------------------------------
  ## --> if a string =====================================================================

  if (!is.data.frame(x)) {
    ## -> if not a dataframe
    # print('change/put the string into a dataframe')

    ## check the number of characters in the sentence
    if(nchar(x) > 750){
      message(paste0("The length of your input text reached the limit in PCRE, ",
                     "please split your input text into shorts ones for another try.",
                     "Idealy, `nchar(x)` should smaller than 750. "))
    }


    df <- data.frame(col = x) %>%
      dplyr::mutate(id = dplyr::row_number())

    code <- df %>%
      dplyr::mutate(sdgs = '') ## for later use, to append data to this column

    for (i in 1:nrow(SDG_keys)){                           ## loop each SDG indicators
      sdg_i_str <- SDG_keys$SDG_id[i] %>% as.character()   ## get the SDG id name
      sdg_i_obj <- SDG_keys$SDG_keywords[i]                ## get the corresponding SDG search term list

      # print(sdg_i_str)
      # print(sdg_i_obj)

      code <-  code %>%
        as.data.frame() %>%

      ## at the sentence level - count once if goals/targets are mentioned ---------------
      dplyr::mutate(
        match = ifelse(
          grepl(pattern = sdg_i_obj, x = col, ignore.case = T, perl = T), 1, 0))                    %>% ## yes-1 or no-0 if they match
          # grepl(pattern = sdg_i_obj, x = as.character({{col}}), ignore.case = T, perl = T), 1, 0))  %>% ## yes-1 or no-0 if they match
        dplyr::mutate(
          sdgs  = ifelse(match > 0, paste0(sdgs, ',', sdg_i_str), sdgs)) %>%
        dplyr::select(-match) %>% ## remove this column
        dplyr::mutate(sdgs = gsub("^,*|(?<=,),|,*$", "", sdgs, perl=T)) %>%
        as.data.frame()
    }


    ## --> if a dataframe ================================================================
  } else {
    ##
    df <- x %>%
      dplyr::mutate(id = dplyr::row_number())

    ## check the number of characters in the sentence
    df_nchar <- df %>%
      dplyr::mutate(nchr = stringr::str_length(col)) %>%
      dplyr::filter(nchr > 750)
    if(nrow(df_nchar) > 0){
      message(paste0("The length of your input text reached the limit in PCRE, ",
                     "please split your input text in rows ",
                     paste(unique(df_nchar$id), collapse = ", "),
                     " into shorts ones for another try.",
                     "Idealy, `nchar(x)` should smaller than 750. "))
    }

    code <- df %>%
      dplyr::mutate(sdgs = '') ## for later use, to append data to this column

    for (i in 1:nrow(SDG_keys)){                           ## loop each SDG indicators
      sdg_i_str <- SDG_keys$SDG_id[i] %>% as.character()   ## get the SDG id name
      sdg_i_obj <- SDG_keys$SDG_keywords[i]                ## get the corresponding SDG search term list

      # print(sdg_i_str)
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
      dplyr::mutate(sdgs = gsub("^,*|(?<=,),|,*$", "", sdgs, perl=T)) %>%
      as.data.frame()
    }
  }



  ### sort from most SDG hits to least (or, none)
  coded <- code %>% dplyr::arrange(desc(nchar(sdgs)), id)

  return(coded)

}
