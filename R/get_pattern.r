#' get_pattern() generates a string to represent a response pattern
#'
#' @param data a data frame, where rows = observations, and columns = variables
#' @param variables selected variables to produce a response pattern
#' @return mplus_object generated with MplusAutomation
#' @export
#'
#' @examples
#'
#' data_with_pattern <- get_pattern(
#' data = data_model,
#' variables  = c('f1','f2','f3','f4','f5','f6')
#' )
#'
#'
get_pattern <- function(data, variables){

pattern <- data %>%
           dplyr::select(one_of(variables)) %>%
           tidyr::unite('pattern', sep = '.', dplyr::everything()) %>%
           dplyr::pull()

return(pattern)


}