#' plot_missing_pattern() generates a plot with missing patterns using VIM::aggr
#'
#' @param data a data frame, where rows = observations, and columns = variables
#' @param scale_num a number, that identifies a unique set of items within the scale_info table
#' @param scale_info a data frame where items memmbership to scale is uniquely identify
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
plot_missing_pattern <- function(data, scale_num, scale_info){


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

set.seed(20210306)

data_plot <- data_selected %>%
             group_by(id_k) %>%
             dplyr::sample_n(100, weight = ws) %>%
             ungroup() %>%
             dplyr::select(-id_k, -ws)

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
