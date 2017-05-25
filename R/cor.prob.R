#' @export
#' @title Correlation matrix
#' @description Correlation Matrix with strength and significance
#' @export
#' @source https://stat.ethz.ch/pipermail/r-help/2001-November/016201.html
#'
#'
#'

cor.prob <- function(X, dfr = nrow(X) - 2) {
  R <- cor(X)
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr / (1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  R
}
