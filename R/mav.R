#' @title Moving average over a continuous variable
#' @description Apply a moving average over a variable defaulting to a window of 5
#' @export
#'
#' @param x Continuous variable over which to apply the moving average
#' @param n number of observation over which to take the moving average
#'
#' @return The moving (or rolling) average of x over the n window
#'
#' @source http://druedin.com/2012/08/11/moving-averages-in-r/

mav <- function(x,n=5){stats::filter(x,rep(1/n,n), sides=2)}
