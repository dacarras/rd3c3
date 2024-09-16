#' pa_mrfa() calculates a parallel analysis using a response data frame
#'
#' @param data a data frame, where rows = observations, and columns = variables
#' @param scale_num a number, that identifies a unique set of items within the scale_info table
#' @param scale_info a data frame where items memmbership to scale is uniquely identify
#' @param n_sim specify the number of simulated correlations, n_sim = 500 is recommended
#' @return a data frame generated with DA.MRFA::parallelMRFA
#' @export
#'
#' @examples
#'
#' pa_mrfa(
#' data = scale_data,
#' scale_num = 1,
#' scale_info = items_data,
#' n_sim = 500
#' ) %>%
#' knitr::kabel(., digits = 2)
#'
#'
pa_mrfa <- function(data, scale_num, scale_info, n_sim){

# -----------------------------------------------
# objects
# -----------------------------------------------

data_model     <- data
number_of_sim  <- n_sim
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

design_data <- data_model %>%
               dplyr::select(id_k, ws)

items_data <- data_model %>%
              rename_at(vars(pre_names), ~paste0(new_names)) %>%
              mutate_at(
              	.vars = reverse_items,
              	.funs = ~r4sda::reverse(.)) %>%
              dplyr::select(one_of(item_names))

data_selected <- dplyr::bind_cols(
                 design_data, items_data)

# -----------------------------------------------
# set seed
# -----------------------------------------------

set.seed(20200220)

# -----------------------------------------------
# create random sample
# -----------------------------------------------

items_data_n500 <- data_selected %>%
                   na.omit() %>%
                   group_by(id_k) %>%
                   dplyr::sample_n(500, weight = ws) %>%
                   ungroup()

# -----------------------------------------------
# keep items only
# -----------------------------------------------

items_n500 <- items_data_n500 %>%
              dplyr::select(-id_k, -ws) %>%
              as.data.frame()


# -----------------------------------------------
# get parallel analysis
# -----------------------------------------------


parallel_results <- DA.MRFA::parallelMRFA(items_n500,
                    Ndatsets = number_of_sim,
                    percent= 95 ,
                    corr = 'Polychoric',
                    display = FALSE,
                    graph = FALSE)

# Note: the first version was implemented with
#       EFA.MRFA::parallelMRFA function. However,
#       the calculations couldn't be completed with this version.
#       As such, we reverted the function, to run with the earlier version
#       DA.MRFA::parallelMRFA().

# -----------------------------------------------
# get amount of expected factors
# -----------------------------------------------

line_p95  <- parallel_results$N_factors_percentiles
line_mean <- parallel_results$N_factors_mean


# -----------------------------------------------
# get data
# -----------------------------------------------

pa_data <- data.frame(
           factors = seq(1:length(parallel_results$Real_Data)),
           sample_data = parallel_results$Real_Data,
           rand_mean = parallel_results$Mean_random,
           rand_p95 = parallel_results$Percentile_random
           ) %>%
           tidyr::gather(key = 'source', value = 'ecv', -factors) %>%
           mutate(ecv = ecv/100) %>%
           mutate(n_factor = line_p95)

# -----------------------------------------------
# returned object
# -----------------------------------------------

return(pa_data)
}
