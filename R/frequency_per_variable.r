#' frequency_per_variable() calculates proportions of category use, from a polytomous scale
#'
#' @param data a data frame, where rows = observations, and columns = variables
#' @param scale_num a number, that identifies a unique set of items within the scale_info table
#' @param scale_info a data frame where items memmbership to scale is uniquely identify
#' @return data frame with proportions and histograms, using skimr::inline_hist
#' @export
#'
#' @examples
#'
#' frequency_per_variable(
#' data = scale_data,
#' scale_num = 1,
#' scale_info = items_data
#' ) %>%
#' knitr::kabel(., digits = 2)
#'
#'
frequency_per_variable <- function(data, scale_num, scale_info){

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
                 dplyr::filter(recoding == 'reverse') %>%
                 dplyr::select(item) %>%
                 .$item
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

design_data <- data_model %>%
               dplyr::select(id_k, ws)

vairables_data <- data_model %>%
                  dplyr::select(one_of(variable_names))

items_data <- data_model %>%
              rename_at(vars(all_of(pre_names)), ~paste0(new_names)) %>%
              mutate_at(
              	.vars = reverse_items, 
              	.funs = ~r4sda::reverse(.)) %>%
              dplyr::select(one_of(item_names))

data_selected <- dplyr::bind_cols(
                 design_data, vairables_data)

# -----------------------------------------------
# critical code
# -----------------------------------------------
x <- data_selected %>%
     dplyr::select(-id_k, -ws)

wide_resp <- function(x){
  # remove warnings
  options(warn=-1)

  # load dplyr
  require(dplyr)

  # get table for histogram
  y <- as.data.frame(x)

  # recode all missing
  x <- mutate_all(x, funs(replace(., is.na(.), -999)))

  # order of table
  order_table <- data.frame(
    variable = as.character(names(x)),
    var_order = seq(1:length(names(x)))
  )

  # freq_table
  table_freq <- function(x){
    as.data.frame(table(x))
  }

  # histograms
  get_hist <- function(x){
    wide_table <- x %>%
      r4sda::remove_labels() %>%
      summarise_all(list(
        hist = ~skimr::inline_hist(.)
      ))
    hist_table <- data.frame(
      variable = as.character(names(x)),
      hist = as.character(tidyr::gather(wide_table)$value)
    )
    return(hist_table)
  }

  # create stacked table
  table <- lapply(x, table_freq) %>%
    dplyr::bind_rows(., .id = 'var') %>%
    rename(resp = x, n = Freq) %>%
    group_by(var) %>%
    mutate(per = n/sum(n)) %>%
    mutate(resp = as.character(resp)) %>%
    mutate(resp = case_when(
      nchar(resp)==1 ~ paste0(0,resp),
      TRUE ~ as.character(resp))) %>%
    mutate(resp = if_else(resp=='-999','NA',resp)) %>%
    arrange(resp) %>%
    dplyr::select(var, resp, per)

  # wide variable table
  wide_resp <- tidyr::spread(table, resp, per) %>%
    rename(variable = var) %>%
    left_join(., order_table, by = 'variable') %>%
    left_join(.,get_hist(y), by = 'variable') %>%
    arrange(var_order) %>%
    dplyr::select(-var_order) %>%
    mutate(hist = as.character(hist))

  return(wide_resp)
  options(warn=0)
}


# -----------------------------------------------
# return
# -----------------------------------------------

return(wide_resp(x))

}