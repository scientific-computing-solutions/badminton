% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eventSim.R
\name{crossOver_functional}
\alias{crossOver_functional}
\title{Create a function for parameterizing cross over probabilities}
\usage{
crossOver_functional(parameterName, rate, shape = 1)
}
\arguments{
\item{parameterName}{Parameter name for the cross over probability}

\item{rate}{rate parameter for hazard function}

\item{shape}{Weibull shape parameter for hazard function}
}
\value{
A function to be used as the rate argument to the InsertRates.EventSim fnction
}
\description{
See Vignette tutorials 4 and 6 for further details
}

