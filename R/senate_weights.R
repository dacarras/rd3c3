#' senate_weights() computes senate weights. These weights are used often to include more than one country with different sample size, yet to scale their weight to a common total.
#'
#' @param data data frame object which contains the rest of the arguments
#' @param wt observations final weight
#' @param id_k a numeric vector of a single variable, to distinguish between countries
#' @param scale number to which the weights will be normalized to
#'
#' @return the original data frame with the new senate weights at the end, 'ws'
#'
#' @details a collection of dplyr::mutate sequences to create normalized and effective sample weights (see Rabe-Hesketh & Skrondal, 2006; Snijder & Bosker, 2012)
#'
#' @examples
#' library(dplyr)
#' data_frame_with_weights <- data_frame %>%
#'                            r4sda::senate_weights(.,
#'                            scale = 1000,
#'                            wt = 'wt',
#'                            id_k = 'id_k')
#' @export
senate_weights <- function(data, wt, scale, id_k){

# libraries
require(dplyr)

# weight scale (up to what sum the weights are normalized to)
k_weight <- scale


# create data frame to produce weights
data_frame  <- data.frame(
               id_k = as.numeric(data[[id_k]]),
               wt = as.numeric(data[[wt]])
               )

require(dplyr)
data_weights <- data_frame %>%
                mutate(sum_w = ave(wt, id_k, FUN = sum)) %>%
                mutate(rscale_f = k_weight/sum_w)             %>%
                mutate(ws = wt*rscale_f)                      %>%
                mutate(ws = haven::zap_labels(ws))            %>%
                dplyr::select(ws) %>%
                tibble::as_tibble()
# add to original data
data_output <- dplyr::bind_cols(data, data_weights)

# code steps

# How to create a senate weight

# 1. Select the constant to which the sum of
#    the weights will be rescaled (in our example, “K”).

# 2. Compute the sum of the sampling weights within each
#    of the groups within which the rescaling will be done.

# 3. Multiply the sample weights by the result of dividing
#    the constant selected in Step 1 above by the sum of
#    the weights in Step 2.

# SOURCE:
# Gonzalez, E. J. (2012). Rescaling sampling weights and selecting
#      mini-samples from large-scale assessment databases.
#      IERI Monograph Series Issues and Methodologies in
#       Large-Scale Assessments, 5, 115–134.


return(data_output)
}
