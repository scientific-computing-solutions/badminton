% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rateSwitchTimes.R
\name{ValidateSwitchTimes}
\alias{ValidateSwitchTimes}
\title{Check the times (both patient and calendar) for hazard function switch are valid}
\usage{
ValidateSwitchTimes(times)
}
\arguments{
\item{times}{a numeric vector of times at which hazard functions are to change}
}
\value{
a sorted vector of times
}
\description{
A vector is valid if all elements are positive and unique. If the times are valid
the time "0" is appended to the vector and the vector is sorted in ascending order
}

