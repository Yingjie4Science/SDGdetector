#' Extract text data from PDF documents, clean, format, and save the text data into a dataframe
#'
#'
#' @param filename The path and name of a PDF file
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select left_join group_by
#' @importFrom stringr str_replace_all str_squish left_join group_by
#' @importFrom tabulizer extract_text
#' @importFrom tabulizerjars
#'
#' @return
#' @export


### Custom function
pdf2text <- function(filename){

  ## read and clean the pdf ------------------------------------------------------------ #
  txt <- tabulizer::extract_text(file = filename) %>%
    iconv("UTF8", "ASCII", "") %>%                 # change encoding
    paste(sep = " ") %>%
    stringr::str_replace_all("\\.com", "") %>%     # avoid matching Comoros (iso3 code: COM)
    stringr::str_replace_all(fixed("\n"), " ") %>% # erase new lines
    stringr::str_replace_all(fixed("\r"), " ") %>% # erase carriage return
    stringr::str_replace_all(fixed("\t"), " ") %>% # erase tabs
    stringr::str_replace_all(fixed("\b"), " ") %>% # erase backspace
    stringr::str_replace_all(fixed("\""), " ") %>% # erase "
    paste(sep = " ", collapse = " ") %>%
    stringr::str_squish() %>%                      # remove leading & trailing whitespace, and repeated whitespace inside a string
    stringr::str_replace_all("- ", "")             # fix incomplete words continuing to next line



  ## text to DF ------------------------------------------------------------------------ #
  ### separate the large character chunk to short sentences by
  ###    '.' (period), ';' (semicolon), ':' (colon), and '/ ' (slash with space)
  px <- paste(".", ";", "/", ":", "?", "!", sep = "\\s|\\"); px
  p0 <- paste0("\\", px); p0
  ### --- split if there is a space between two numbers; e.g., "2012 3.1" --> "2012", "3.1"
  p1 <- '(?<=\\d)\\s\\d'   ## best, only keep the number before that whitespace
  ### --- split if "." followed by a non-number character, e.g., "of a primary school.During 2018" (space is accidentally missing)
  p2 <- '\\.(?=\\D)'
  pat <- paste(
    paste0('(', p1, ')'),
    # paste0('(', p2, ')'),
    paste0('(', p0, ')'),
    sep = "|")

  txt_sentence <- unlist(strsplit(txt, split = pat, perl=T)) %>% trimws()

  txt_sentence_df <- data.frame(statement = txt_sentence) %>%
    dplyr::mutate(
      id = row.names(.) %>% as.numeric(),
      statement = as.character(statement)) %>%
    as.data.frame()

  return(txt_sentence_df)
}



