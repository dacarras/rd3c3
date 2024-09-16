get_omega_h <- function(model){

require(dplyr)
require(stringr)

library(dplyr)
factor_var <- model %>%
              purrr::pluck('results') %>%
              purrr::pluck('parameters') %>%
              purrr::pluck('stdyx.standardized') %>%
              dplyr::filter(grepl('.BY', paramHeader)) %>%
              mutate(factor = stringr::str_remove(paramHeader, '.BY')) %>%
              mutate(lambda = est) %>%
              rename(parcel = param) %>%
              group_by(factor) %>%
              mutate(var_factor = sum(lambda)^2) %>%
              ungroup() %>%
              rename(term = factor) %>%
              rename(variance = var_factor) %>%
              dplyr::select(parcel, term, variance)

library(dplyr)
uniqueness <- model %>%
              purrr::pluck('results') %>%
              purrr::pluck('parameters') %>%
              purrr::pluck('stdyx.standardized') %>%
              dplyr::filter(grepl('Residual.Variance', paramHeader)) %>%
              mutate(epsilon = est) %>%
              rename(parcel = param) %>%
              mutate(var_unique = sum(epsilon)) %>%
              mutate(term = 'epsilon') %>%
              mutate(variance = var_unique) %>%
              dplyr::select(parcel, term, variance)
              


omega_table <- dplyr::bind_rows(factor_var, uniqueness) %>%
               dplyr::select(term, variance) %>%
               unique() %>%
               mutate(var_total = sum(variance)) %>%
               mutate(estimate = variance/var_total) %>%
               dplyr::filter(term == 'FG') %>%
               mutate(index = 'omega_h') %>%
               mutate(factor = term) %>%
               dplyr::select(index, estimate)

return(omega_table)

}