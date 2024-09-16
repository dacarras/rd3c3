#' std_eta() scales a theta score, or any other score into a standardized score, relying on the the survey design of the data
#'
#' @param data a data frame, containing id_j, ws, and id_s vectors for survey design
#' @param eta a IRT or LVM realization (or any other score)
#' @return a numeric vector with  with mean == 0 and sd == 1, at the population level
#' @export
#'
#' @examples
#'
#' score_01 <- data_response %>%
#'             mutate(std_bel = std_eta(., eta_score))
#'
#'
std_eta <- function(data, eta){

  require(dplyr)
  require(srvyr)
  require(survey)


  library(srvyr)
  scores_svy <- data %>%
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

  std_score <- data %>%
    mutate(score = (eta-center)/std_dev) %>%
    dplyr::select(score)  %>%
    dplyr::pull()

  return(std_score)
}