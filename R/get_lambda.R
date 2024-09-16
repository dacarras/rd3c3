#' get_lambda() retrieves the factor loadings of a latent variable model
#'
#' @param model it uses an input a mplus_object generated with MplusAutomation
#'
#' @return data frame with with descriptive values
#' @export
#'
#' @examples
#'
#' get_lambda(model)
#'
#'
get_lambda <- function(model){

require(dplyr)
require(stringr)

lambda_table <- model %>%
                purrr::pluck('results') %>%
                purrr::pluck('parameters') %>%
                purrr::pluck('stdyx.standardized') %>%
                dplyr::filter(grepl('.BY', paramHeader)) %>%
                mutate(factor = stringr::str_sub(paramHeader, 1, 3)) %>%
                rename(item = param) %>%
                mutate(factor = stringr::str_sub(paramHeader, 1, 3)) %>%
                rename(item = param) %>%
                dplyr::select(item, est, se, est_se, pval)

return(lambda_table)

}
