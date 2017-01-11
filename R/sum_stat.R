#' @title Standard Error
#' @export
#' @description Calculate the standard error of a distribution.
#' @param x distribution over which to calculate the standard error
#' @note the variance of the sample is calculate with na.rm=TRUE
#' @examples
#' x <- 1:10
#' se(x=x)
se <- function(x) sqrt(var(x, na.rm=TRUE)/length(x)) # note NA remove







