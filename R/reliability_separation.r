#' reliability_separation() calculates separation reliability of theta scores realizations
#'
#' @param model it uses an input a mplus_object generated with MplusAutomation
#' @return a numeric object
#' @export
#'
#' @examples
#'
#' reliability_separation(scale_001)
#'
#'
reliability_separation <- function(model){

require(dplyr)
mplus_object <- model

theta_var   <- mplus_object %>%
               purrr::pluck('results') %>%
               purrr::pluck('savedata') %>%
               dplyr::select('ETA') %>%
               .$ETA %>%
               var()

within_var   <- mplus_object %>%
               purrr::pluck('results') %>%
               purrr::pluck('savedata') %>%
               dplyr::select('ETA_SE') %>%
               mutate(within_var = ETA_SE^2) %>%
               .$within_var %>%
               mean()

total_var   <- theta_var + within_var
reliability <- theta_var/total_var

return(reliability)
}