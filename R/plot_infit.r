#' plot_infit() plot infit results from a Rasch model over a response data frame
#'
#' @param data a data frame, where rows = observations, and columns = variables
#' @param scale_num a number, that identifies a unique set of items within the scale_info table
#' @param scale_info a data frame where items memmbership to scale is uniquely identify
#' @return a base plot
#' @export
#'
#' @examples
#'
#' plot_infit(
#' data = scale_data,
#' scale_num = 1,
#' scale_info = items_data
#' ) %>%
#' knitr::kabel(., digits = 2)
#'
#'
plot_infit <- function(data, scale_num, scale_info){

# -----------------------------------------------
# objects
# -----------------------------------------------

data_model     <- data
scales_data    <- scale_info
scales_id      <- scale_num


# -----------------------------------------------
# quiet all functions
# -----------------------------------------------

quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

# -----------------------------------------------
# item vectors
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
              rename_at(vars(pre_names), ~paste0(new_names)) %>%
              mutate_at(
              	.vars = reverse_items,
              	.funs = ~r4sda::reverse(.)) %>%
              dplyr::select(one_of(item_names))

data_selected <- items_data

# -----------------------------------------------
# critical code
# -----------------------------------------------

# prepare data
items_tam <- data_selected
min_cat   <- min(data_selected, na.rm = TRUE)

if (min_cat == 0 ) {

  items_data_tam <- items_tam

  } else {

  items_data_tam <- na.omit(items_tam-1)
}


tam_00 <- TAM::tam.mml(
	      resp=items_data_tam,
	      irtmodel = "1PL",
	      verbose = FALSE)

tam_00_fit <- quiet(TAM::tam.fit(tam_00))

library(dplyr)
item_fit_infit <- tam_00_fit$itemfit %>%
                  dplyr::filter(
                    abs(Infit_t) >2 & Infit > 1.33 |
                    abs(Infit_t) >2 & Infit <  .75)

infit_data <- dplyr::select(tam_00_fit$itemfit, parameter, Infit)


dotchart(infit_data$Infit, labels = infit_data$parameter,
         cex = 0.6, xlab = "Infit", xlim = c(0,2),
         main = 'Weighted mean square, items residuals')
abline(v=c(.75, 1.33), lty = 3)

}
