
## load database -------------------------------------------------------------------------
library(dplyr)
load('./data/list_of_un_goals_targets.rda')
UN_SDGs <- list_of_un_goals_targets %>%
  dplyr::mutate(
    goalname = paste0('SDG', GoalID),
    target_id_un = paste0('SDG', target_id_un),
    id = row_number()
  ) %>%
  dplyr::select(goalname, target_id_un, id) %>%
  as.data.frame()
rm(list_of_un_goals_targets)



#' Summarize results from SDGdetector at either the Goal level or Target level.
#'
#' @param data   Data frame or a string
#' @param sum_by The group level to be chosen for data summary. Default parameter is
#'    "target", and can also set at "goal" level.
#' @param quiet Logical. Suppress info message
#'
#' @return
#' Data frame with at least one column named "SDG" or "Target", and one column `Freq` that
#' represent the total hits.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select filter rename row_number everything arrange
#' @importFrom tidyr separate
#' @importFrom utils data
#'
#' @export
#'
#' @examples
#' library(SDGdetector)
#' df <- data.frame(col = c(
#'     'our goal is to end poverty globally',
#'     'this product contributes to slowing down climate change'))
#' data <- SDGdetector(x = df, col = col)
#' summarize_sdg(data, sum_by = 'target', quiet = FALSE)
#'
summarize_sdg <- function(data, sum_by = 'target', quiet = FALSE) {

  coded <- data %>% as.data.frame()

  ### --> pull out the column with coded results
  # coded_sdgs <- dplyr::pull(coded, sdgs)   ## the same as below one
  coded_sdgs <- coded['sdgs']
  # nn <- ncol(coded)
  # coded_sdgs <- coded[nn];
  coded_sdgs <- unlist(coded_sdgs)
  coded_sdgs <- as.vector(coded_sdgs)
  coded_sdgs <- paste(coded_sdgs, sep = " ", collapse = ",")
  # coded_sdgs


  ### --> format it as a DF
  coded_sdgs <- unlist(strsplit(coded_sdgs, split = "\\,"))
  coded_sdgs <- trimws(coded_sdgs);

  coded_sdgs_df <- as.data.frame(table(coded_sdgs)) %>%
    dplyr::filter(coded_sdgs != '') %>%
    tidyr::separate(col = coded_sdgs, into = c('goal', 'target_id'), sep = '_', remove = F) %>%
    dplyr::mutate(
      goal_id    = as.numeric(gsub("\\D", "", goal)),
      target_id  = gsub('^.{3}', "", coded_sdgs),
      target_id  = gsub('_', ".", target_id),
      coded_sdgs = gsub('_', ".", coded_sdgs),
      ) %>%
    arrange(goal_id, target_id) %>%
    dplyr::select(1:2, goal_id, everything())


  ### --> The above data only presents what were detected, but some SDGs without match won't be shown.
  ### --> Force to list all 17 SDGs and 169 Targets
  ### --> merge data
  coded_sdgs_df_format <-
    merge(x = coded_sdgs_df, ## the coded data
          y = UN_SDGs,      ## the UN goal and target id for formatting
          by.x = c('goal', 'coded_sdgs'),
          by.y = c('goalname', 'target_id_un'), all = T) %>%
    dplyr::mutate(goal = factor(goal, levels = paste0('SDG', 1:17))) %>%
    arrange(goal, id) %>%
    dplyr::mutate(id = row_number()) %>%
    ## for the NA in `goal_id` and `target_id`, fill the info
    dplyr::mutate(
      goal_id   = ifelse(is.na(goal_id),   gsub('SDG|\\_.*$','', goal), goal_id),
      target_id = ifelse(is.na(target_id), gsub('SDG','', coded_sdgs),  target_id) ## Remove all text before '_'
    ) %>%
    dplyr::rename('target' = 'coded_sdgs') %>%
    dplyr::select(-id) %>%
    as.data.frame()


  ### summarize data by group ------------------------------------------------------------
  if (sum_by == 'target') {
    ### group and sum by `target`
    coded_sdgs_df_sum <- coded_sdgs_df_format %>%
      group_by(goal, target) %>%
      dplyr::summarise_at(c("Freq"), sum, na.rm = TRUE) %>%
      dplyr::rename('SDG' = 'goal', 'Target' = 'target')

  } else if (sum_by == 'goal') {
    coded_sdgs_df_sum <- coded_sdgs_df_format %>%
      group_by(goal) %>%
      dplyr::summarise_at(c("Freq"), sum, na.rm = TRUE) %>%
      dplyr::rename('SDG' = 'goal')

  } else {
    print('Please specify `sum_by` with either "goal", or "target" for data summary.')
  }


  ###
  return(coded_sdgs_df_sum)

}
