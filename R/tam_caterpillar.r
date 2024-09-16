#' tam_caterpillar() fits a rash models with TAM and generates a caterpillar plot with its theta realizations
#'
#' @param data a data frame, where rows = observations, and columns = variables
#' @param scale_num a number, that identifies a unique set of items within the scale_info table
#' @param scale_info a data frame where items memmbership to scale is uniquely identify
#' @return a caterpillar plot generated with library(ggplot2)
#' @export
#'
#' @examples
#'
#' missing_summary(
#' data = scale_data,
#' scale_num = 1,
#' scale_info = items_data
#' ) %>%
#' knitr::kabel(., digits = 2)
#'
#'
tam_caterpillar <- function(data, scale_num, scale_info){

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
             dplyr::select(variable) %>%
             .$variable

new_names <- scales_data %>%
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
# critical code
# -----------------------------------------------

items_tam <- na.omit(items_data-1)

tam_00 <- TAM::tam.mml(
	      resp=items_tam, 
	      irtmodel = "1PL",
	      verbose = FALSE
	      )

tam_wle  <- TAM::tam.wle(tam_00, progress = FALSE)

theta_data <- tam_wle %>%
         mutate(ylo = theta - 1.96*error) %>%
         mutate(yhi = theta + 1.96*error) %>%
         mutate(x = pid)   %>%
         mutate(y = theta) %>%
         dplyr::select(x, y, ylo, yhi)

sample_theta <- dplyr::sample_n(tbl=theta_data, size=100, replace = FALSE)

require(ggplot2)
plot <- ggplot(sample_theta, 
	        aes(x=reorder(x,y), 
	        	y=y, 
	        	ymin=ylo, 
	        	ymax=yhi))+
        geom_pointrange(colour='grey20', size = .2)+
        geom_hline(yintercept = .00, linetype=2, size = .25, colour = "grey50")+
        ylim(c(-5,5)) +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(axis.text.y = element_text(size=6, colour = "grey50")) +
        theme(axis.text.x = element_blank()) +
        theme(axis.ticks = element_blank())+
        xlab('persons') +
        ylab(expression(theta[p])) +
        theme(
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major.y = element_line(size = .05, colour = "grey50"),
        panel.grid.minor.y = element_line(size = .05, colour = "grey50")
        )
 
return(plot)

}