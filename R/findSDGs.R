

## 1. This function will be used to identify matches ############################################# #

# THIS CODE IS TO IDENTIFY MATCHING ROWS, BUT IT DOES NOT EXTRACT SENTENCES
# in the original dataframe, if it matches, then in a new column, say yes; if not, say no.

sdg_detector <- function(dataframe, company_name) {

  code <- dataframe %>%
    dplyr::mutate(#match   = 0,
      sdgs    = '', ## for later use, to append data to this column
      n_total = 0,
      sdgs_n  = '')

  for (i in 1:nrow(SDG_keys)){                ## all SDG indicators
    sdg_i_str <- SDG_keys$SDG_id[i]         ## the SDG id name
    sdg_i_obj <- SDG_keys$SDG_keywords[i]   ## the corresponding SDG search term list

    print(sdg_i_str)
    # print(sdg_i_obj)

    code <- code %>% as.data.frame() %>%
      ## at the sentence level - count once ----------------------
    dplyr::mutate(
      match = ifelse(
        grepl(pattern = sdg_i_obj, x = statement, ignore.case = T, perl = T), 1, 0))  %>% ## yes-1 or no-0 if they match
      dplyr::mutate(sdgs = ifelse(match > 0, paste0(sdgs, ',', sdg_i_str), sdgs)) %>%

      ## at the sentence level - count all matches ---------------
    dplyr::mutate(
      n       = str_count(string = statement, regex(pattern = sdg_i_obj, ignore_case = T)),
      n_total = n_total + n,
      sdgs_n  = ifelse(n > 0, paste0(sdgs_n, ',', sdg_i_str, '-', n), sdgs_n)) %>%
      as.data.frame()
  }


  ### sort from most SDG hits to least (or, none)
  coded <- code %>% arrange(desc(nchar(sdgs)), id)

  ### save all the hits to xlsx for easier inspection.
  # fname <- paste0(dirpath, 'DF_coded/', company_name, '_coded.xlsx'); fname
  # writexl::write_xlsx(x = coded, path = fname)
  fname <- paste0('./data/output/', company_name, '_coded.csv'); fname
  readr::write_csv(x = coded, file = fname)
  fname <- paste0('./data/output/', company_name, '_coded.RData'); fname
  save(coded, file = fname)

  return(coded)

}
