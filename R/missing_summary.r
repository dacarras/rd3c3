#' missing_summary() generates missing summary
#'
#' @param data a data frame, where rows = observations, and columns = variables
#' @param scale_num a number, that identifies a unique set of items within the scale_info table
#' @param scale_info a data frame where items memmbership to scale is uniquely identify
#' @return a table of missing, distinguishing between complete response, partial, and fully missing
#' @export
#'
#' @examples
#'
#' missing_summary(
#' data = scale_data,
#' scale_num = 1,
#' scale_info = items_data
#' ) %>%
#' knitr::kabel(., digits = 2)
#'
#'
missing_summary <- function(data, scale_num, scale_info){

# -----------------------------------------------
# objects
# -----------------------------------------------

data_model     <- data
scales_data    <- scale_info
scales_id      <- scale_num

# -----------------------------------------------
# set seed
# -----------------------------------------------

item_names <- scales_data %>%
              dplyr::filter(scale_num == scales_id) %>%
              dplyr::select(item) %>%
              .$item %>%
              as.character()

reverse_items <- scales_data %>%
                 dplyr::filter(scale_num == scales_id) %>%
                 dplyr::filter(recoding == 'reverse') %>%
                 dplyr::select(item) %>%
                 .$item
# -----------------------------------------------
# get data for modelling
# -----------------------------------------------

pre_names <- scales_data %>%
             dplyr::filter(scale_num == scales_id) %>%
             dplyr::select(variable) %>%
             .$variable

new_names <- scales_data %>%
             dplyr::filter(scale_num == scales_id) %>%
             dplyr::select(item) %>%
             .$item

items_data <- data_model %>%
              rename_at(vars(pre_names), ~paste0(new_names)) %>%
              mutate_at(
              	.vars = reverse_items,
              	.funs = ~r4sda::reverse(.)) %>%
              dplyr::select(one_of(item_names))

# -----------------------------------------------
# scale number of items
# -----------------------------------------------

scale_length <- ncol(items_data)

# -----------------------------------------------
# get summary table
# -----------------------------------------------

summary <- items_data %>%
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
