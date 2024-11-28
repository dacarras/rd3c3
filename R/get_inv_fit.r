#' get_inv_fit() retrieves model fit indexes from GRM models to generate the invariance table
#'
#' @param model a response model, with a MplusAutomation model object
#' @param model_name argument to define the fitted model (e.g., pooled, strict, scalar, configural, base)
#' @export
#'
#' @examples
#'
#' fit_1 <- get_inv_fit(
#'          model = inv_1, 
#'          model_name = 'strict'
#'          )
#'
get_inv_fit <- function(model, model_name){
library(dplyr)

error_text <- model$results$errors %>% 
              unlist() %>% 
              paste(., collapse = "\n")

error_terms <- c('NO CONVERGENCE', 'NONCONVERGENCE')

error_test <-  sum(stringr::str_detect(error_text, error_terms)) >= 1

  if (dplyr::if_else(error_test == TRUE, FALSE, TRUE)) {
  fit_indexes <- model %>%
  purrr::pluck('results') %>%
  purrr::pluck('summaries') %>%
  dplyr::select(ChiSqM_Value, ChiSqM_DF, ChiSqM_PValue, Parameters, CFI, TLI, RMSEA_Estimate, SRMR) %>%
  rename(
    x2    = ChiSqM_Value,
    df    = ChiSqM_DF,
    p_val = ChiSqM_PValue,
    parameters = Parameters,
    CFI = CFI,
    TLI = TLI,
    RMSEA = RMSEA_Estimate,
    SRMR = SRMR,
    ) %>%
  mutate(model = model_name) %>%
  dplyr::select(
   model, x2, df, p_val, parameters, CFI, TLI, RMSEA, SRMR      
    )
  }
      else {
  fit_indexes <- data.frame(
    model =  model_name,
    x2 = NA_real_,
    df = NA_real_,
    p_val = NA_real_,
    parameters = NA_real_,
    CFI = NA_real_,
    TLI = NA_real_,
    RMSEA = NA_real_,
    SRMR = NA_real_
  )
  }

fit_table <- fit_indexes

return(fit_table)

# Note: the invariance function is not retrieving fit indexes, for
#       non-convergent models. Yet, there are other troublesome 
#       errors, such as non-positive definite matrixes.

}
