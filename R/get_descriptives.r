#' get_descriptives() produces a descriptive table from a response data frame
#'
#' @param data a data frame, where rows = observations, and columns = variables
#' @param scale_num a number, that identifies a unique set of items within the scale_info table
#' @param scale_info a data frame where items memmbership to scale is uniquely identify
#' @return data frame with descriptives estimates using libraries such as stats, moments, and skimr
#' @export
#'
#' @examples
#'
#' get_descriptives(
#' data = scale_data,
#' scale_num = 1,
#' scale_info = items_data
#' ) %>%
#' knitr::kabel(., digits = 2)
#'
#'
get_descriptives <- function(data, scale_num, scale_info){

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

variables_data <- data_model %>%
                  dplyr::select(one_of(variable_names))

items_data <- data_model %>%
              rename_at(vars(pre_names), ~paste0(new_names)) %>%
              mutate_at(
              	.vars = reverse_items,
              	.funs = ~r4sda::reverse(.)) %>%
              dplyr::select(one_of(item_names))

data_selected <- variables_data

# -----------------------------------------------
# critical code
# -----------------------------------------------
x <- data_selected

options(warn=-1)

  # requires
  library(dplyr)
  library(r4sda)
  library(skimr)
  library(tidyr)
  library(stats)

remove_labels <- function(data){
require(haven)
require(labelled)
require(sjlabelled)
data <- data %>%
          haven::zap_label() %>%
          labelled::remove_labels() %>%
          sjlabelled::remove_all_labels()
  return(data)
}

  # histograms
  get_hist <- function(x){
    wide_table <- x %>%
      remove_labels() %>%
      summarise_all(list(
        hist = ~skimr::inline_hist(.)
      ))
    hist_table <- data.frame(
      var = names(x),
      hist = tidyr::gather(wide_table)$value
    ) %>%
      mutate(var = as.character(var))
    return(hist_table)
  }

  # minimum
  get_p00 <- function(x){
    wide_table <- x %>%
      remove_labels() %>%
      summarise_all(list(
        p00 = ~stats::quantile(., probs = 0, na.rm = TRUE, names = FALSE)
      ))
    min_table <- data.frame(
      var = names(x),
      p00 = tidyr::gather(wide_table)$value
    ) %>%
      mutate(var = as.character(var))
    return(min_table)
  }

  # maximum
  get_p100 <- function(x){
    wide_table <- x %>%
      remove_labels() %>%
      summarise_all(list(
        p100 = ~stats::quantile(., probs = 1, na.rm = TRUE, names = FALSE)
      ))
    max_table <- data.frame(
      var = names(x),
      p100 = tidyr::gather(wide_table)$value
    ) %>%
      mutate(var = as.character(var))
    return(max_table)
  }

  # mean
  get_mean <- function(x){
    wide_table <- x %>%
      remove_labels() %>%
      summarise_all(list(
        mean = ~mean(., na.rm = TRUE)
      ))
    mean_table <- data.frame(
      var = names(x),
      mean = tidyr::gather(wide_table)$value
    ) %>%
      mutate(var = as.character(var))
    return(mean_table)
  }

  # standard deviation
  get_sd <- function(x){
    wide_table <- x %>%
      remove_labels() %>%
      summarise_all(list(
        sd = ~stats::sd(., na.rm = TRUE)
      ))
    sd_table <- data.frame(
      var = names(x),
      sd = tidyr::gather(wide_table)$value
    ) %>%
      mutate(var = as.character(var))
    return(sd_table)
  }

  # median
  get_p50 <- function(x){
    wide_table <- x %>%
      remove_labels() %>%
      summarise_all(list(
        p50 = ~stats::quantile(., probs = .50, na.rm = TRUE, names = FALSE)
      ))
    median_table <- data.frame(
      var = names(x),
      p50 = tidyr::gather(wide_table)$value
    ) %>%
      mutate(var = as.character(var))
    return(median_table)
  }

  # p25
  get_p25 <- function(x){
    wide_table <- x %>%
      remove_labels() %>%
      summarise_all(list(
        p25 = ~stats::quantile(., probs = .25, na.rm = TRUE, names = FALSE)
      ))
    p25_table <- data.frame(
      var = names(x),
      p25 = tidyr::gather(wide_table)$value
    ) %>%
      mutate(var = as.character(var))
    return(p25_table)
  }

  # p75
  get_p75 <- function(x){
    wide_table <- x %>%
      remove_labels() %>%
      summarise_all(list(
        p75 = ~stats::quantile(., probs = .75, na.rm = TRUE, names = FALSE)
      ))
    p75_table <- data.frame(
      var = names(x),
      p75 = tidyr::gather(wide_table)$value
    ) %>%
      mutate(var = as.character(var))
    return(p75_table)
  }

  # missing
  get_missing <- function(x){
    wide_table <- x %>%
      remove_labels() %>%
      summarise_all(list(
        missing = ~sum(is.na(.))
      ))
    missing_table <- data.frame(
      var = names(x),
      missing = tidyr::gather(wide_table)$value
    ) %>%
      mutate(var = as.character(var))
    return(missing_table)
  }

  # get number of cases
  get_n <- function(x){
    wide_table <- x %>%
      remove_labels() %>%
      summarise_all(list(
        nobs = ~NROW(.)
      ))
    nobs_table <- data.frame(
      var = names(x),
      n = tidyr::gather(wide_table)$value
    ) %>%
      mutate(var = as.character(var))
    return(nobs_table)
  }

  # get complete
  get_complete <- function(x){
    wide_table <- x %>%
      remove_labels() %>%
      summarise_all(list(
        complete = ~sum(complete.cases(.))
      ))
    complete_table <- data.frame(
      var = names(x),
      complete = tidyr::gather(wide_table)$value
    ) %>%
      mutate(var = as.character(var))
    return(complete_table)

  }

  # skewness
  get_skew <- function(x){
    wide_table <- x %>%
      remove_labels() %>%
      summarise_all(list(
        skew = ~moments::skewness(., na.rm = TRUE)
      ))
    p75_table <- data.frame(
      var = names(x),
      skew = tidyr::gather(wide_table)$value
    ) %>%
      mutate(var = as.character(var))
    return(p75_table)
  }

  # kurtosis
  get_kurt <- function(x){
    wide_table <- x %>%
      r4sda::remove_labels() %>%
      summarise_all(list(
        kurt = ~moments::kurtosis(., na.rm = TRUE)
      ))
    p75_table <- data.frame(
      var = names(x),
      kurt = tidyr::gather(wide_table)$value
    ) %>%
      mutate(var = as.character(var))
    return(p75_table)
  }
  # get wide table
  wide_table <- get_missing(x) %>%
    dplyr::left_join(.,get_complete(x), by = 'var') %>%
    dplyr::left_join(.,get_n(x), by = 'var') %>%
    mutate(missing = missing/n) %>%
    mutate(complete = complete/n) %>%
    dplyr::left_join(.,get_mean(x), by = 'var') %>%
    dplyr::left_join(.,get_sd(x), by = 'var') %>%
    dplyr::left_join(.,get_p00(x), by = 'var') %>%
    dplyr::left_join(.,get_p25(x), by = 'var') %>%
    dplyr::left_join(.,get_p50(x), by = 'var') %>%
    dplyr::left_join(.,get_p75(x), by = 'var') %>%
    dplyr::left_join(.,get_p100(x), by = 'var') %>%
    dplyr::left_join(.,get_skew(x), by = 'var') %>%
    dplyr::left_join(.,get_kurt(x), by = 'var') %>%
    dplyr::left_join(.,get_hist(x), by = 'var')

  options(warn=0)


# -----------------------------------------------
# return
# -----------------------------------------------

return(wide_table)

}
