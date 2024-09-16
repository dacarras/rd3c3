#' plot_missing_summary() it plots missing summary
#'
#' @param missing_summary a data frame generated with techr::missing_summary
#' @return a plot generated with library(ggplot2) and library(waffle)
#' @export
#' @source \url{https://www.r-bloggers.com/round-values-while-preserve-their-rounded-sum-in-r/} and
#' \url{http://stackoverflow.com/questions/32544646/round-vector-of-numerics-to-integer-while-preserving-their-sum}
#' @examples
#'
#' missing_summary(
#' data = scale_data,
#' scale_num = 1,
#' scale_info = items_data
#' ) %>%
#' plot_missing_summary()
#'
#'
plot_missing_summary <- function(missing_summary){

round_preserve_sum <- function(x, digits) {
  up <- 10^digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x - y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y / up
}

# require libraries
require(ggplot2)
require(waffle)
require(hrbrthemes)
require(dplyr)

square_pie <- missing_summary %>%
mutate(pattern = factor(pattern, levels = c("missing", "partial", "complete"))) %>%
mutate(percent = as.numeric(round_preserve_sum(percent, 2))*100) %>%
as.data.frame() %>%
  ggplot(aes(fill = pattern, values = percent)) +
  waffle::geom_waffle(n_rows = 10, size = 0.33, colour = "white", flip = TRUE, na.rm = TRUE) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "missing"  = "grey10",
      "partial"  = "grey60",
      "complete" = "grey90"
      )
  ) +
  coord_equal() +
  hrbrthemes::theme_ipsum_rc(grid="") +
  theme_enhance_waffle()

return(square_pie)

}


