% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sum_stat.R
\name{se}
\alias{se}
\title{Standard Error}
\usage{
se(x)
}
\arguments{
\item{x}{distribution over which to calculate the standard error}
}
\description{
Calculate the standard error of a distribution.
}
\note{
the variance of the sample is calculate with na.rm=TRUE
}
\examples{
x <- 1:10
se(x=x)
}
