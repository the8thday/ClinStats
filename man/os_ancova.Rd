% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/os_ancova.R
\name{os_ancova}
\alias{os_ancova}
\title{One step ANCOVA analysis}
\usage{
os_ancova(
  data,
  outcome,
  group,
  covariate,
  type = "III",
  plot = TRUE,
  check_model = TRUE
)
}
\arguments{
\item{data}{data frame contains needed variable}

\item{outcome}{outcome}

\item{group}{group}

\item{covariate}{covariate}

\item{type}{Anova type,default='III'}

\item{plot}{whether show plots,TRUE of FALSE}

\item{check_model}{whether check the model}
}
\value{
a list contains all you need
}
\description{
\code{os_ancova} is a one step ANCOVA analysis function to produce results as SPSS.
}
\examples{
data(ancova_data)
os_ancova(data = ancova_data, outcome = 'diffW24', group = 'group', covariate = 'W1D1')
}
