#' pa_lubbe() calculates a parallel analysis using a response data frame
#'
#' @param data a data frame, where rows = observations, and columns = variables
#' @param scale_num a number, that identifies a unique set of items within the scale_info table
#' @param scale_info a data frame where items memmbership to scale is uniquely identify
#' @param n_sim specify the number of simulated correlations, n_sim = 1000 is recommended
#' @return a data frame generated depicting observed and simulated eigenvalues, and number of factors
#' @export
#'
#' @examples
#'
#' pa_lubbe(
#' data = scale_data,
#' scale_num = 1,
#' scale_info = items_data,
#' n_sim = 1000
#' ) %>%
#' knitr::kabel(., digits = 2)
#'
#'
pa_lubbe <- function(data, scale_num, scale_info, n_sim){

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
              	.funs = ~reverse(.)) %>%
              dplyr::select(one_of(item_names))

data_selected <- dplyr::bind_cols(
                 design_data, items_data)

# -----------------------------------------------
# set seed
# -----------------------------------------------

set.seed(20200220)


# -----------------------------------------------
# keep items only
# -----------------------------------------------

data_pa <- data_selected %>%
           dplyr::select(-id_k, -ws) %>%
           as.data.frame() 

# -----------------------------------------------
# UCP adjusted parallel analysis function
# -----------------------------------------------

pa_ucp_lubbe <- function(data, replications=1000, prc=0.5) {

require(psych)
require(dplyr)

p <- ncol(data)
n <- nrow(data)


#------------------------------------------------
# 1. Calculate UCPs
#------------------------------------------------

ucp <- list()
for(j in 1:p) {
freq <- table(data[,j])
ucp[[j]] <- c(0, cumsum(freq) / sum(freq))
}

ev <- matrix(, replications, p)
for(i in 1:replications) {

#------------------------------------------------
# 2. Generate Random Samples
#------------------------------------------------

  x <- y <- matrix(rnorm(n*p), n, p)
  for(j in 1:p) {
#------------------------------------------------
# # 3. Discretization of Random Variables
#------------------------------------------------
    splt <- quantile(x[,j], probs = ucp[[j]])
    for(k in 1:(length(splt)-1))
    y[which(x[,j] >= splt[k] & x[,j] <= splt[k+1]), j] <- k
}

#------------------------------------------------
# 4. Calculate Polychoric Correlations
#------------------------------------------------

R <- polychoric(y)$rho

#------------------------------------------------
# 5. Calculate Eigenvalues
#------------------------------------------------

ev[i,] <- eigen(R)$values
}

pa_results <- data.frame(
sample_ev = eigen(polychoric(data)$rho)$values,
reference_ev = apply(ev, 2, quantile, probs = prc)
) %>%
mutate(factors = seq(1:nrow(.))) %>%
mutate(flag = dplyr::if_else(sample_ev > reference_ev, 1, 0)) %>%
mutate(possible_factors = case_when(
flag == 1 ~ factors,
flag == 0 ~ 0
)) %>%
mutate(n_factors = max(possible_factors, na.rm = TRUE)) %>%
dplyr::select(sample_ev, reference_ev, factors, n_factors)

return(pa_results)

# Parallel analysis as indicated in Lubbe (2019).

}

# Univariate Categorical Probability adjusted Parallel Analysis
# (Lubbe, 2019)
#
# References
#
# Lubbe, D. (2019). Parallel analysis with categorical variables: 
#    Impact of category probability proportions on dimensionality 
#    assessment accuracy. Psychological Methods, 24(3), 339–351. 
#    https://doi.org/10.1037/met0000171

# -----------------------------------------------
# get amount of expected factors
# -----------------------------------------------

pa_lubbe <- data_pa %>%
pa_ucp_lubbe(data = .,
replications = number_of_sim,
prc = .5)


# -----------------------------------------------
# returned object
# -----------------------------------------------

return(pa_lubbe)
}
