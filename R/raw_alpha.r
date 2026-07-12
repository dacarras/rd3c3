#' raw_alpha() calculates the alpha coefficient from a response table.
#'
#' @param data a data frame, where rows = observations, and columns = variables
#' @param scale_num a number, that identifies a unique set of items within the scale_info table
#' @param scale_info a data frame where items memmbership to scale is uniquely identify
#' @return a numeric object, calculated with psych::alpha
#' @export
#'
#' @examples
#'
#' raw_alpha(
#' data = scale_data,
#' scale_num = 1,
#' scale_info = items_data
#' ) %>%
#' print
#'
#'
raw_alpha <- function(data, scale_num, scale_info){

# -----------------------------------------------
# objects
# -----------------------------------------------

responses      <- data
scales_data    <- scale_info
selected_scale <- scale_num

# -----------------------------------------------
# item list
# -----------------------------------------------

item_names <- scales_data %>%
              dplyr::filter(scale_num == selected_scale) %>%
              dplyr::select(item) %>%
              .$item %>%
              as.character()

# -----------------------------------------------
# get data for modelling
# -----------------------------------------------

pre_names <- scales_data %>%
             dplyr::filter(scale_num == selected_scale) %>%
             dplyr::select(variable) %>%
             .$variable

new_names <- scales_data %>%
             dplyr::filter(scale_num == selected_scale) %>%
             dplyr::select(item) %>%
             .$item

reverse_items <- scales_data %>%
                 dplyr::filter(scale_num == selected_scale) %>%
                 dplyr::filter(recoding == 'reverse') %>%
                 dplyr::select(item) %>%
                 .$item

items_data <- responses %>%
              rename_at(vars(pre_names), ~paste0(new_names)) %>%
              mutate_at(
                .vars = reverse_items,
                .funs = ~reverse(.)) %>%
              dplyr::select(one_of(item_names))

data_model <- dplyr::bind_cols(
              items_data)

# -----------------------------------------------
# critical code
# -----------------------------------------------

raw_alpha <- data_model %>%
             as.data.frame() %>%
             psych::alpha(., check.keys = FALSE) %>%
             purrr::pluck('total') %>%
             .$raw_alpha


return(raw_alpha)

}