#' plot_missing_pattern_plain() generates a plot with missing patterns using VIM::aggr
#'
#' @param data a data frame, where rows = observations, and columns = variables
#' @param scale_num a number, that identifies a unique set of items within the scale_info table
#' @param scale_info a data frame where items memmbership to scale is uniquely identify
#' @param n_sample define number of random sample
#' @return a plot of missing patters generated with VIM::aggr
#' @export
#'
#' @examples
#'
#' plot_missing_pattern(
#' data = data_responses,
#' scale_num = 1,
#' scale_info = scales_data
#' )
#'
plot_missing_pattern_plain <- function(data, scale_num, scale_info, n_sample){


# -----------------------------------------------
# quiet all functions
# -----------------------------------------------

quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

# -----------------------------------------------
# objects
# -----------------------------------------------

data_model     <- data
scales_data    <- scale_info
scales_id      <- scale_num
sample_size    <- n_sample

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

# -----------------------------------------------
# critical code
# -----------------------------------------------

set.seed(20210306)

data_plot <- items_data %>%
             dplyr::sample_n(sample_size)

plot_missing <- data_plot %>%
VIM::aggr(.,
  combine=TRUE,
  cex.lab=.8,
  cex.axis=.6,
  col = c("white", "black"),
  ylabs="Pattern of omissions"
  )

# -----------------------------------------------
# return
# -----------------------------------------------

return(quiet(plot_missing))

}
