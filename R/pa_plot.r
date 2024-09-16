#' pa_plot() plot the results of a parallel analysis generated with the pa_mrfa() function
#'
#' @param data it uses an input a pa_mrfa() results, generated with DA.MRFA::parallelMRFA
#' @return a ggplot object
#' @export
#'
#' @examples
#'
#' plot_error(pa_mrfa_data)
#'
#'
pa_plot <- function(data){

plot_data <- data
n_factor  <- data %>%
             dplyr::select(n_factor) %>%
             unique() %>%
             .$n_factor %>%
             as.numeric()

require(ggplot2)
plot <- ggplot(plot_data, aes(x=factors, y=ecv, group=source)) +
  geom_line(aes(linetype=source)) +
  geom_point() +
  scale_x_continuous(
    name ='Number of factors',
    limits = c(1,nrow(plot_data)/3), 
    breaks = seq(from = 1, to = nrow(plot_data)/3, by = 1)
    ) +
  scale_y_continuous(
    name ='Explained Common Variance',
    limits = c(0,1), 
    breaks = c(0,.25, .50, .75, 1)
    ) +
  scale_linetype_manual(
    values=c("dashed","dotted","solid"),
    name = "", 
    labels = c("Random Mean", "Random P95", "Observed Data")) +
  theme_minimal() +
  theme(
    panel.grid.major.y=element_blank(),
    panel.grid.major.x=element_blank(),
    panel.grid.minor.y=element_blank(),
    panel.grid.minor.x=element_blank(),    
    legend.position= c(0.8, .9)
    ) + 
  geom_vline(
    xintercept = n_factor, 
    linetype=2, 
    size = .25, 
    color = 'grey40') 

return(plot)
}