#' get_theta() retrieves the theta score generated with techr::fit_grm2 or techr::fit_pcm
#'
#' @param model an mplus object generated with techr::fit_grm2 or techr::fit_pcm
#' @param name the name of the score included in the returned data frame
#' @return data frame with id_i and a score vector with the defined name
#' @export
#'
#' @examples
#'
#' theta_01 <- fit_grm2(
#'             scale_info = scales_data,
#'             scale_num  = 1,
#'             data = data_model) %>%
#'             get_theta(., 'score')
#'
#'
get_theta <- function(model, name){

  require(dplyr)
  require(purrr)

  score_name <- name

  theta_score <- model %>%
    purrr::pluck('results') %>%
    purrr::pluck('savedata') %>%
    rename_all(tolower) %>%
    dplyr::select(id_i, eta)

  names(theta_score) <- c('id_i',score_name)

  return(theta_score)

}
