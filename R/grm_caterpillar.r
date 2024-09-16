#' grm_caterpillar() produces a caterpillar plot of theta realizations
#'
#' @param model it uses an input a mplus_object generated with MplusAutomation
#' @return a numeric object
#' @export
#'
#' @examples
#'
#' grm_caterpillar(scale_001)
#'
#'
grm_caterpillar <- function(model){

require(dplyr)
mplus_object <- model

theta_data <- mplus_object %>%
              purrr::pluck('results') %>%
              purrr::pluck('savedata') %>%
              dplyr::select(ETA, ETA_SE, WS) %>%
              rename_all(tolower) %>%
              tibble::as_tibble() %>%
              mutate(y = eta) %>%
              mutate(ylo = eta - 1.96*eta_se) %>%
              mutate(yhi = eta + 1.96*eta_se) %>%
              mutate(x = seq(1:nrow(.)))   %>%
              dplyr::select(x, y, ylo, yhi, ws)

set.seed(20210309)

sample_theta <- dplyr::sample_n(tbl=theta_data,
                weight = ws,
                size=100,
                replace = FALSE)

require(ggplot2)

caterpillar_plot <- ggplot(sample_theta,
  aes(x=reorder(x,y), y=y, ymin=ylo, ymax=yhi)) +
  geom_pointrange(colour='grey20', size = .2)+
  geom_hline(yintercept = .00, linetype=2, size = .25, colour = "grey50")+
  ylim(c(-5,5)) +
  xlab('persons') +
  ylab(expression(theta[p])) +
  theme(
  panel.background = element_rect(fill = "white", colour = "grey50"),
  panel.grid.major.y = element_line(size = .05, colour = "grey50"),
  panel.grid.minor.y = element_line(size = .05, colour = "grey50"),
  axis.text.y = element_text(size=6, colour = "grey50"),
  axis.text.x = element_blank(),
  axis.ticks = element_blank(),
  plot.title = element_text(hjust = 0.5)
  )

return(caterpillar_plot)


}
