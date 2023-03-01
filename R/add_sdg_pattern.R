## helper functions ----------------------------------------------------------------------

## a vector (v) of terms to be concatenated by `AND`

func_AND_vector <- function(v){
  pat <- paste0("(?=.*(?:", v, "))", collapse="")
  return(pat)
}


## a vector (v) of terms to be concatenated by `OR`
func_OR_vector <- function(v){
  pat <- paste0(v, collapse = "|")
  pat <- paste0("(", pat, ")")
  # print(pat)
  return(pat)
}

## loas database -------------------------------------------------------------------------
load('data/SDG_keys.rda')
sdg_id_list <- unique(SDG_keys$SDG_id)


## define the function to add patterns to the existing database --------------------------

#' Users Can Add Customized Patterns for Each SDG or Target
#'
#' @param x        A vector of strings
#' @param sdg_id   SDG Goal's ID or Target's ID, in the format of 'SDGx_y', e.g., SDG1_1, SDG2_general
#' @param operator 'AND', 'OR' to combine a vector of keywords for identifying SDG Goals or Targets.
#' @param quiet Logical. Suppress info message
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select left_join group_by
#'
#' @return A regerx string
#' @export
#'
#' @examples
#' terms_new <- c("improve", "farmer", "income")
#' add_sdg_pattern(sdg_id = 'SDG1_2', x = terms_new, operator = 'AND')
#'
add_sdg_pattern <-
  function(sdg_id,
           x,
           operator = 'AND',
           quiet = FALSE) {
    ## check the format of `sdg_id`
    if (any(!sdg_id %in% sdg_id_list)) {
      stop(
        paste0(
          "sdg_id names must be in the right format that similar to ",
          "'SDG1_1', 'SDG12_3', or 'SDG2_general'"
        )
      )
    }


    if (length(x) < 2) {
      new_pattern <- func_OR_vector(x)
    } else if (length(x) > 1 & operator == 'AND') {
      new_pattern <- func_AND_vector(x)
    } else if (length(x) > 1 & operator == 'OR') {
      new_pattern <- func_OR_vector(x)
    }

    new_pattern_df <- data.frame(SDG_id       = sdg_id,
                                 SDG_keywords = new_pattern,
                                 match_tpye   = 'user_defined')

    cat('New pattern for detecting SDGs was added: \n')
    print(new_pattern_df)

    ## update the search term database
    SDG_keys <- rbind(SDG_keys,
                      new_pattern_df) %>%
      dplyr::distinct_all()

    # return(SDG_keys)
    invisible(SDG_keys)
  }


## test ----------------------------------------------------------------------------------
# x <- "improve"
# x <- c("improve", "farmer", "income")
# add_sdg_pattern(sdg_id = 'SDG1_2', x = x, operator = 'AND')
# SDG_keys <- add_sdg_pattern(sdg_id = 'SDG1_1', x = x, operator = 'AND')
