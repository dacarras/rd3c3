get_iecv <- function(model){

require(dplyr)
require(stringr)

lambda_table <- model %>%
              purrr::pluck('results') %>%
              purrr::pluck('parameters') %>%
              purrr::pluck('stdyx.standardized') %>%
              dplyr::filter(grepl('.BY', paramHeader)) %>%
              mutate(factor = stringr::str_sub(paramHeader, 1, 2)) %>%
              rename(item = param) %>%
              dplyr::select(item, factor, est) %>%
              tidyr::spread(factor, est)


iecv_table <- model %>%
              purrr::pluck('results') %>%
              purrr::pluck('parameters') %>%
              purrr::pluck('stdyx.standardized') %>%
              dplyr::filter(grepl('.BY', paramHeader)) %>%
              mutate(factor = stringr::str_sub(paramHeader, 1, 2)) %>%
              mutate(lamda_sq = est*est) %>%
              mutate(item_var = r4sda::c_sum(lamda_sq, param)) %>%
              mutate(iECV = lamda_sq/item_var) %>%
              rename(item = param) %>%
              dplyr::filter(factor == 'FG') %>%
              dplyr::select(item, iECV)
              
iecv_est <- lambda_table %>%
            dplyr::left_join(.,
              iecv_table,
              by = 'item') %>%
            tibble::as_tibble()

return(iecv_est)

}