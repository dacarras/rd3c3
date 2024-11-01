#' silent() suppresses and hides other functions messages and warnings. Its ideal use if to wrapped functions within dynamic reports.
#'
#' @param x a function wrapping its inputs
#'
#' @return a the product of the wrapped function without messages and warnings
#'
#' @examples
#'
#' table <- items_data %>%
#'          psych::alpha() %>%
#'          silent()
#'
#' @export
silent <- function(x){

quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
}

return(suppressWarnings({suppressMessages({quiet(x)})}))
}
