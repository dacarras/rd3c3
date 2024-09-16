#' plot_error_plain() plot a scatter of errors and theta locations with their histograms, using library(ggplot2)
#'
#' @param model it uses an input a mplus_object generated with MplusAutomation
#' @return a ggplot object
#' @export
#'
#' @examples
#'
#' plot_error(scale_001)
#'
#'
plot_error_plain <- function(model){

mplus_object <- model

data_eta    <- mplus_object %>%
               purrr::pluck('results') %>%
               purrr::pluck('savedata') %>%
               dplyr::select(ETA, ETA_SE) %>%
               dplyr::rename_all(tolower)


library(ggplot2)
scatter_plot <- ggplot(data_eta, aes(eta, eta_se)) + 
     geom_point(
     	shape = 20,
     	aes(alpha = 1/10000),
     	size = 3,
     	colour = 'grey20',
     	show.legend = FALSE
     	) + 
     theme_minimal() + 
     theme(
  panel.background = element_rect(fill = NA,
                                  linetype = 'dashed',
                                  colour = 'grey20',
                                  size = 0.25
                                 ),
  panel.grid.major = element_line(size = 0.25, 
  	                              linetype = 'dashed',
                                  colour = 'grey20'
                                  ), 
  panel.grid.minor = element_blank(),
  panel.border     = element_rect(linetype = 'solid', fill = NA)
  ) +
  scale_x_continuous(
  	name = expression(theta),
  	limits = c(-4, 4),
  	breaks = seq(-4,4,1)) +
  scale_y_continuous(
  	name = expression('SE'(theta)),
  	limits = c(0, 1),
  	breaks=seq(0,1,.25)
  	)

ggExtra::ggMarginal(scatter_plot,
	type = 'histogram',
	colour = 'grey20',
	alpha = .15,
	fill = 'grey90')
}