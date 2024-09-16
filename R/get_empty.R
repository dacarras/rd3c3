#' get_empty() estimates the amount of cases with empty rows
#'
#' @param data a data frame responses, where rows = observations, and columns = variables
#' @return a table of missing, distinguishing missing by 
#' @export
#'
#' @examples
#'
#' data_with_countries %>%
#' dplyr::select(countries, item_001:item_050) %>%
#' split(.$countries) %>%
#' purrr::map( ~ get_empty(.)) %>%
#' dplyr::bind_rows(., .id = 'IDCNTRY')
#'
#'
get_empty <- function(data){

data_selected <- data
scale_length <- ncol(data_selected)

summary <- data_selected %>%
           mutate(na_count = apply(., 1, function(x) sum(is.na(x)))) %>%
           mutate(empty = case_when(
            na_count == (scale_length - 1) ~ 1,
            TRUE ~ 0
            )) %>%
           summarize(
             empty_n = as.numeric(sum(empty, na.rm=TRUE)),
             cases_n = as.numeric(nrow(data_selected)),
             empty_p = as.numeric(empty_n/nrow(data_selected))
             )

return(summary)
}

