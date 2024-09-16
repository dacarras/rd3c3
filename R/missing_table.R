#' missing_table() generates missing summary
#'
#' @param data a data frame, where rows = observations, and columns = variables
#' @return a table of missing, distinguishing between complete response, partial, and fully missing
#' @export
#'
#' @examples
#'
#' missing_table(
#' data = scale_data
#' ) %>%
#' knitr::kabel(., digits = 2)
#'
#'
missing_table <- function(data){

  # -----------------------------------------------
  # scale number of items
  # -----------------------------------------------

  scale_length <- ncol(data)

  # -----------------------------------------------
  # get summary table
  # -----------------------------------------------

  summary <- data %>%
    mutate(na_count = apply(., 1, function(x) sum(is.na(x)))) %>%
    mutate(pattern = case_when(
      between(na_count, scale_length, scale_length) ~ 'missing',
      between(na_count, 0, 0) ~ 'complete',
      between(na_count, 1, scale_length-1) ~ 'partial'
    )) %>%
    dplyr::count(pattern) %>%
    mutate(percent = n/sum(n))

  part_1 <- summary %>%
    dplyr::filter(pattern == 'complete')

  part_2 <- summary %>%
    dplyr::filter(pattern == 'partial')

  part_3 <- summary %>%
    dplyr::filter(pattern == 'missing')


  summary <- dplyr::bind_rows(part_1, part_2, part_3)

  # -----------------------------------------------
  # return
  # -----------------------------------------------

  return(summary)

}
