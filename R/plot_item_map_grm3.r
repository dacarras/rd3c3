#' plot_item_map_grm3() generates an item person map using a GRM model with 4 response categories
#'
#' @param model it uses an input a mplus_object generated with MplusAutomation
#' @param scale_num a number, that identifies a unique set of items within the scale_info table
#' @param scale_info a data frame where items memmbership to scale is uniquely identify
#' @return a item map plot generated with library(ggplot2)
#' @export
#'
#' @examples
#'
#' plot_item_map_grm3(scales_data, 2, scale_002)
#'
#'
plot_item_map_grm3 <- function(scale_info, scale_num, model){

# -----------------------------------------------
# removes warnings
# -----------------------------------------------
options(warn = -1)

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
scale_info   <- scale_info
scale_id     <- scale_num

# -----------------------------------------------
# get items table
# -----------------------------------------------

item_table <- scale_info %>%
              dplyr::filter(scale_num == scale_id) %>%
              dplyr::select(item, item_text)

# -----------------------------------------------
# get length parameters
# -----------------------------------------------

max_cat <- paste0(nrow(item_table),'k')
max_pos <- nrow(item_table)

low_limit <- -3
upp_limit <-  3

# -----------------------------------------------
# get scale name
# -----------------------------------------------

scale_name <- scale_info %>%
              dplyr::filter(scale_num == scale_id) %>%
              dplyr::select(scale_name) %>%
              unique() %>%
              .$scale_name

# -----------------------------------------------
# get tau parameters
# -----------------------------------------------

tau_est <- mplus_object %>%
           purrr::pluck('results') %>%
           purrr::pluck('parameters') %>%
           purrr::pluck('unstandardized') %>%
           dplyr::filter(paramHeader %in% c('Thresholds')) %>%
           tidyr::separate(param, c('item', 'cat'), sep = "[^[:alnum:]]+") %>%
           mutate(steps = paste0('c',cat)) %>%
           mutate(item = stringr::str_replace(item, "[^[:alnum:]]", '')) %>%
           dplyr::select(item, steps, est) %>%
           tidyr::spread(steps, est) %>%
           tidyr::gather(key = 'cat', value = 'logit', -item)


tau_order <- tau_est %>%
             dplyr::filter(cat == 'c1') %>%
             arrange(desc(logit)) %>%
             mutate(x_pos = seq(1:nrow(.))) %>%
             dplyr::select(item, x_pos)


plot_data <- dplyr::left_join(
             tau_est,
             tau_order,
             by = 'item')  %>%
             rename_all(tolower) %>%
             mutate(item = tolower(item)) %>%
             dplyr::left_join(.,
             item_table,
             by = 'item')  %>%
             mutate(item_text = case_when(
             cat == 'c1' ~ '',
             TRUE ~ item_text)) %>%
             mutate(order_plot = case_when(
              cat == 'c2' ~ '1',
              cat == 'c1' ~ '2'
              )) %>%
              as.data.frame()


item_text_plot <- plot_data %>%
                  arrange(order_plot, x_pos) %>%
                  dplyr::select(item_text) %>%
                  dplyr::pull()


# -----------------------------------------------
# get theta scores
# -----------------------------------------------

theta_p <- mplus_object %>%
           purrr::pluck('results') %>%
           purrr::pluck('savedata') %>%
           dplyr::rename_all(tolower) %>%
           dplyr::select(eta) %>%
           as.data.frame()

# -----------------------------------------------
# position for text
# -----------------------------------------------

x_pos <- plot_data %>%
         dplyr::select(x_pos) %>%
         unique() %>%
         .$x_pos

# not in use
category_location <- plot_data %>%
                     dplyr::filter(x_pos == max(x_pos)) %>%
                     mutate(plot_location = logit + .25) %>%
                     dplyr::select(plot_location) %>%
                     dplyr::bind_rows(.,
                      data.frame(plot_location = - low_limit)) %>%
                     arrange(plot_location) %>%
                     .$plot_location %>%
                     as.vector()

# -----------------------------------------------
# histogram
# -----------------------------------------------

library(ggpubr)
library(ggplot2)
p1 <- gghistogram(theta_p,
      x = "eta",
      fill = "grey90",
      binwidth = .2,
      ggtheme = theme_bw()
      ) +
      rremove('x.grid') +
      xlab('') + ylab(expression(theta[p])) +
    scale_x_continuous(
      limits = c(low_limit, upp_limit),
      breaks = seq(low_limit, upp_limit, 1))


# -----------------------------------------------
# dotplot
# -----------------------------------------------


library(ggpubr)
library(ggplot2)
p2 <- ggdotchart(plot_data, x = "item", y = "logit",
   group = "order_plot", color = "order_plot",
   palette = c('grey50','black', 'grey70'),
   rotate = TRUE,
   sorting = 'asc',
   ggtheme = theme_bw(),
   y.text.col = FALSE ) +
   rremove('x.grid') +
   rremove('legend') +
   xlab(bquote(tau['1k'] ~ "-" ~ tau[.(max_cat)])) +
   ylab(scale_name) +
   geom_text(
    aes(
      label = item_text_plot,
      y = low_limit,
      x = x_pos - .25),
      colour = "grey50",
      size = 3,
      hjust = 0
      ) +
    scale_y_continuous(
      limits = c(low_limit, upp_limit),
      breaks = seq(low_limit, upp_limit, 1))


# -----------------------------------------------
# item map
# -----------------------------------------------

item_map <- ggarrange(p1, p2,
            ncol = 1,
            nrow = 2,
            align = 'v')

# -----------------------------------------------
# object
# -----------------------------------------------

return(item_map)

# -----------------------------------------------
# re-stablishes warnings
# -----------------------------------------------
options(warn = 0)


}
