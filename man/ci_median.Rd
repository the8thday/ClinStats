% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CI.R
\name{ci_median}
\alias{ci_median}
\title{calculate median CI for single vector}
\usage{
ci_median(x, conf.level = 0.95)
}
\arguments{
\item{x}{numberic vector}

\item{conf.level}{confidence level of the interval
}
}
\value{
a tibble
}
\description{
calculate median CI for single vector
}
\examples{
ci_median(c(1,2,3,4,5))
}
