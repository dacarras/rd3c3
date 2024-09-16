#' get_fit() produces a table displaying obtained fit statistics
#'
#' @param model it uses an input a mplus_object generated with MplusAutomation
#' @return a table
#' @export
#'
#' @examples
#'
#' get_fit(scale_001)
#'
#'
get_fit <- function(model){

fit_table <- model %>%
             purrr::pluck('results') %>%
             purrr::pluck('summaries') %>%
             tidyr::gather(index, value) %>%
             rename(observed = value) %>%
             dplyr::filter(index %in%
              c('Parameters',
                'ChiSqM_Value',
                'ChiSqM_DF',
                'ChiSqM_PValue',
                'CFI', 
                'RMSEA_Estimate',
                'RMSEA_90CI_LB', 
                'RMSEA_90CI_UB', 
                'SRMR')) %>%
             mutate(index = case_when(
              index == 'Parameters'     ~ 'Parameters',
              index == 'ChiSqM_Value'   ~ 'Chi_sq',
              index == 'ChiSqM_DF'      ~ 'Chi_df',
              index == 'ChiSqM_PValue'  ~ 'Chi_pval',
              index == 'CFI'            ~ 'CFI',
              index == 'RMSEA_Estimate' ~ 'RMSEA',
              index == 'RMSEA_90CI_LB'  ~ 'RMSEA_ci90_ll',
              index == 'RMSEA_90CI_UB'  ~ 'RMSEA_ci90_ul',
              index == 'SRMR'           ~ 'SRMR',)) %>%
             mutate(expected = case_when(
              index == 'CFI'   ~ '>.90',
              index == 'RMSEA' ~ '<.08',
              index == 'SRMR'  ~ '<.08')) %>%
             mutate(strict = case_when(
              index == 'CFI'   ~ '>.95',
              index == 'RMSEA' ~ '<.05',
              index == 'SRMR'  ~ '<.05')) %>%
             mutate(observed = as.numeric(observed))             

return(fit_table)
}