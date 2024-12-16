#' remove_labels() from a data frame
#'
#' @param data data frame with labels
#'
#' @return the original data frame without any labels
#'
#' @examples
#' library(dplyr)
#' data_without_labels <- data_frame %>%
#'                        remove_labels()
#'
#'
#' @export
remove_labels <- function(x){
data <- dplyr::mutate(x, across(everything(), as.vector))
return(data)
}
