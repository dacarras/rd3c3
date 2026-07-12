#' pa_lubbe_fast() calculates a parallel analysis using a response data frame
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
#' pa_lubbe_fast(
#' data = scale_data,
#' scale_num = 1,
#' scale_info = items_data,
#' n_sim = 1000
#' ) %>%
#' knitr::kabel(., digits = 2)
#'
#'
pa_lubbe_fast <- function(data, scale_num, scale_info, n_sim){

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

pa_ucp_lubbe_fast <- function(
    data,
    replications = 1000,
    prc          = 0.5,
    cor_method   = c("psych", "lavaan", "mixed")  # "mixed" = psych for obs, lavaan for sim
) {

  library(psych)
  library(furrr)
  library(matrixStats)

  cor_method <- match.arg(cor_method)

  p <- ncol(data)
  n <- nrow(data)

  # -----------------------------------------------
  # 1. Pre-compute UCPs ONCE
  # -----------------------------------------------
   ucp_probs <- lapply(seq_len(p), function(j) {
    freq <- tabulate(data[, j])
    c(0, cumsum(freq) / sum(freq))
  })

  # -----------------------------------------------
  # 2. Polychoric estimator helpers
  # -----------------------------------------------

  # psych — original, full ML
  poly_psych <- function(mat) {
    tryCatch(
      psych::polychoric(mat, global = FALSE)$rho,
      error = function(e) diag(ncol(mat))
    )
  }

  # lavaan — faster C implementation
  poly_lavaan <- function(mat) {
    tryCatch({
      mat_ord <- as.data.frame(
        lapply(as.data.frame(mat), ordered)
      )
      lavaan::lavCor(
       mat_ord,
       ordered    = names(mat_ord),
       estimator  = "two.step",   # univariate thresholds — same as global=FALSE
       cor.smooth = TRUE          # repair non-PD — same as smooth=TRUE
     )
    },
    error = function(e) diag(ncol(mat))
    )
  }

  # Select estimator for simulated replicates
  poly_sim <- switch(cor_method,
    psych  = poly_psych,
    lavaan = poly_lavaan,
    mixed  = poly_lavaan   # lavaan for sim, psych for observed (see step 6)
  )

  # -----------------------------------------------
  # 3. Vectorized discretization helper
  # -----------------------------------------------
  discretize_col <- function(z, probs) {
    cuts <- quantile(z, probs = probs, type = 1)
    findInterval(z, cuts, rightmost.closed = TRUE)
  }

  # -----------------------------------------------
  # 3b. Detach large enclosing environments
  #     poly_sim/poly_psych/poly_lavaan/discretize_col are
  #     defined inside pa_ucp_lubbe_fast(), whose own enclosing
  #     environment is pa_lubbe_fast()'s frame — which holds
  #     data_model, data_selected, items_data, etc. Without this,
  #     future has to serialize that whole chain to each worker.
  #     These helpers only use their own arguments, so reassigning
  #     a clean environment is safe.
  # -----------------------------------------------
  environment(poly_psych)     <- globalenv()
  environment(poly_lavaan)    <- globalenv()
  environment(poly_sim)       <- globalenv()
  environment(discretize_col) <- globalenv()

  # -----------------------------------------------
  # 4. Set up parallel backend
  # -----------------------------------------------
  plan(multisession, workers = max(1, parallel::detectCores() - 1))

  # -----------------------------------------------
  # 5. Parallelize replications
  # -----------------------------------------------

  ev_list <- furrr::future_map(
    .x = seq_len(replications),

    .f = function(i) {

      x <- matrix(rnorm(n * p), n, p)

      y <- mapply(
        FUN      = discretize_col,
        as.data.frame(x),
        ucp_probs,
        SIMPLIFY = FALSE
      )
      y <- do.call(cbind, y)

      R <- poly_sim(y)

      eigen(R, only.values = TRUE, symmetric = TRUE)$values
    },
    .options = furrr_options(seed = TRUE)
  )

  ev <- do.call(rbind, ev_list)

  # -----------------------------------------------
  # 6. Observed eigenvalues
  #    Always use psych for the real data —
  #    accuracy matters here, not speed
  # -----------------------------------------------
  obs_estimator <- if (cor_method == "mixed") poly_psych else poly_sim

  obs_cor   <- obs_estimator(as.matrix(data))
  sample_ev <- eigen(obs_cor, only.values = TRUE, symmetric = TRUE)$values

  # -----------------------------------------------
  # 7. Reference eigenvalues
  # -----------------------------------------------
  reference_ev <- matrixStats::colQuantiles(ev, probs = prc)

  # -----------------------------------------------
  # 8. Assemble results
  # -----------------------------------------------
  pa_results <- data.frame(
    sample_ev    = sample_ev,
    reference_ev = reference_ev,
    factors      = seq_len(p)
  )

  pa_results$flag             <- as.integer(pa_results$sample_ev > pa_results$reference_ev)
  pa_results$possible_factors <- ifelse(pa_results$flag == 1, pa_results$factors, 0L)
  pa_results$n_factors        <- max(pa_results$possible_factors, na.rm = TRUE)
  pa_results                  <- pa_results[, c("sample_ev", "reference_ev", "factors", "n_factors")]

  plan(sequential)

  return(pa_results)
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
pa_ucp_lubbe_fast(data = .,
replications = number_of_sim,
cor_method = "mixed",
prc = .5
)


# -----------------------------------------------
# returned object
# -----------------------------------------------

return(pa_lubbe)
}