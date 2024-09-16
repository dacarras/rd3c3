#' get_score() retrieves the theta score and re-scale these at mean zero, and sd one at the population level
#'
#' @param model an mplus object generated with techr::fit_grm2 or techr::fit_pcm
#' @param name the name of the score included in the returned data frame
#' @return data frame with id_i and a score vector with mean == 0 and sd == 1, at the population level
#' @export
#'
#' @examples
#'
#' score_01 <- fit_grm2(
#'             scale_info = scales_data,
#'             scale_num  = 1,
#'             data = data_model) %>%
#'             get_score(., 'score')
#'
#'
get_score <- function(model, name){

  require(dplyr)
  require(purrr)
  require(srvyr)
  require(survey)

  theta_score <- model %>%
    purrr::pluck('results') %>%
    purrr::pluck('savedata') %>%
    rename_all(tolower)

  library(srvyr)
  scores_svy <- theta_score %>%
    as_survey_design(
      strata = id_s,
      weights = ws,
      id = id_j)

  library(survey)
  options(survey.lonely.psu = "certainty")

  center <-  scores_svy %>%
    summarize(
      mean = survey_mean(eta,na.rm=TRUE)
    ) %>%
    dplyr::select(mean) %>%
    dplyr::pull()

  std_dev <-  scores_svy %>%
    summarize(
      sd = survey_sd(eta,na.rm=TRUE)
    ) %>%
    dplyr::select(sd) %>%
    dplyr::pull()

  std_score <- theta_score %>%
    mutate(score = (eta-center)/std_dev) %>%
    dplyr::select(id_i, score)

  score_name <- name

  names(std_score) <- c('id_i',score_name)

  return(std_score)
}
