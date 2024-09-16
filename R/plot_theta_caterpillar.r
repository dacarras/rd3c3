#' plot_theta_caterpillar() produces a caterpillar plot for theta scores
#'
#' @param model it uses an input a mplus_object generated with MplusAutomation
#' @return a ggplot object
#' @export
#'
#' @examples
#'
#' plot_theta_caterpillar(scale_001)
#'
#'
plot_theta_caterpillar <- function(model){

# -----------------------------------------------
# requires
# -----------------------------------------------

require(dplyr)
require(tidyr)
require(ggplot2)
require(ggpubr)

# -----------------------------------------------
# require objects
# -----------------------------------------------

mplus_object <- model

# -----------------------------------------------
# get items table
# -----------------------------------------------

eta_data  <- mplus_object %>%
             purrr::pluck('results') %>%
             purrr::pluck('savedata') %>%
             dplyr::rename_all(tolower) %>%
             dplyr::select(eta, eta_se, ws, id_i) %>%
             mutate(ylo = eta - 1.96*eta_se) %>%
             mutate(yhi = eta + 1.96*eta_se) %>%
             mutate(x = id_i)   %>%
             mutate(y = eta)

# -----------------------------------------------
# get length parameters
# -----------------------------------------------
set.seed(20210309)

caterpillar_plot <- eta_data %>%
dplyr::sample_n(tbl=., size=100, weight = ws) %>%
ggplot(., aes(x=reorder(x,y), y=y, ymin=ylo, ymax=yhi))+
 geom_pointrange(colour='grey20', size = .2)+
 geom_hline(yintercept = .00, linetype=2, size = .25, colour = "grey50")+
 ylim(c(-5,5)) +
theme(plot.title = element_text(hjust = 0.5)) +
theme(axis.text.y = element_text(size=06, colour = "grey50")) +
theme(axis.text.x = element_blank()) +
theme(axis.ticks = element_blank())+
xlab('persons') +
ylab(expression(theta[p])) +
theme(
panel.background = element_rect(fill = "white", colour = "grey50"),
panel.grid.major.y = element_line(size = .05, colour = "grey50"),
panel.grid.minor.y = element_line(size = .05, colour = "grey50")
)

# ----------------------------------------------- 
# object
# -----------------------------------------------

return(caterpillar_plot)

}