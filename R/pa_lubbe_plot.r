#' pa_lubbe_plot() plot the results of a parallel analysis generated with the pa_lubb() function
#'
#' @param data it uses an input a pa_lubb() results, UCP adjusted parallel analysis (Lubbe, 2019)
#' @return a ggplot object
#' @export
#'
#' @examples
#'
#' pa_lubbe_plot(pa_lubbe_data)
#'
#'
pa_lubbe_plot <- function(data){

plot_data <- data %>%
             tidyr::gather(
             key = 'source', 
             value = 'eigen_values', 
             -factors, -n_factors
             )

x_axis    <- data %>%
             dplyr::select(factors) %>%
             unique() %>%
             .$factors %>%
             as.numeric()

n_factor  <- data %>%
             dplyr::select(n_factors) %>%
             unique() %>%
             .$n_factors %>%
             as.numeric()

require(ggplot2)
pa_plot <- ggplot(plot_data, aes(x=factors, y=eigen_values, group=source)) +
  geom_line(aes(linetype=source)) +
  geom_point() +
  scale_x_continuous(
    name ='Number of factors',
    limits = c(1,nrow(plot_data)/2), 
    breaks = seq(from = 1, to = max(x_axis), by = 1)
    ) +
  ylab(
    'Eigenvalues') +
#   scale_y_continuous(
#     name ='Eigen Values',
#     limits = c(0,1), 
#     breaks = c(0,.25, .50, .75, 1)
#     ) +
  scale_linetype_manual(
    values=c("dotted","solid"),
    name = "", 
    labels = c("Reference", "Observed Data")) +
   geom_vline(
    xintercept = n_factor, 
    linetype=2, 
    size = .25, 
    color = 'grey40') +
  theme_minimal() +
  theme(
    panel.grid.major.y=element_blank(),
    panel.grid.major.x=element_blank(),
    panel.grid.minor.y=element_blank(),
    panel.grid.minor.x=element_blank(),    
    legend.position= c(0.8, .9),
    panel.border = element_rect(linetype = "solid", fill = NA)
    ) +
  labs(
    caption = 'Source: Univariate Categorical Probability adjusted Parallel Analysis (Lubbe, 2019)'
    )


return(pa_plot)
}
