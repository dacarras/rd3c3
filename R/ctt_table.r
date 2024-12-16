#' ctt_table() produces a table with classical test theory estimates.
#'
#' @param data a data frame, where rows = observations, and columns = variables
#' @param scale_num a number, that identifies a unique set of items within the scale_info table
#' @param scale_info a data frame where items memmbership to scale is uniquely identify
#' @return data frame with CTT estimates, generated with CTT::itemAnalysis()
#' @export
#'
#' @examples
#'
#' ctt_table(
#' data = scale_data,
#' scale_num = 1,
#' scale_info = items_data
#' ) %>%
#' knitr::kabel(., digits = 2)
#'
#'
ctt_table <- function(data, scale_num, scale_info){

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

variable_names <- scales_data %>%
              dplyr::filter(scale_num == scales_id) %>%
              dplyr::select(variable) %>%
              .$variable %>%
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
              rename_at(vars(all_of(pre_names)), ~paste0(new_names)) %>%
              mutate_at(
              	.vars = reverse_items, 
              	.funs = ~reverse(.)) %>%
              dplyr::select(one_of(item_names))

data_selected <- dplyr::bind_cols(items_data)

# -----------------------------------------------
# critical code
# -----------------------------------------------

x <- data_selected
     

library(dplyr)
items_table <- x %>% as.data.frame()

table_1 <- get_desc(items_table)

table_2 <- items_table %>%
           CTT::itemAnalysis(.) %>%
           purrr::pluck('itemReport') %>%
           tibble::as_tibble() %>%
           rename(var = itemName) %>%
           rename(point_biserial = pBis) %>%
           rename(biserial = bis) %>%
           rename(alpha_if_deleted = alphaIfDeleted) %>%
           dplyr::select(var, point_biserial, biserial, alpha_if_deleted)

table_3 <- dplyr::left_join(table_1, table_2, by = 'var') %>%
           dplyr::select(
           var, n, missing, complete, 
           mean, sd, min, max, 
           skew, hist,
           # point_biserial, 
           biserial, 
           alpha_if_deleted)

return(table_3)

}