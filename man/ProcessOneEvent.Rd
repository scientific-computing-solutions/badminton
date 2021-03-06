% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/event.R
\name{ProcessOneEvent}
\alias{ProcessOneEvent}
\title{Process a sngle event for a single patient}
\usage{
ProcessOneEvent(data, recruitTime, event)
}
\arguments{
\item{data}{The output from a event simulation for a single patient Id}

\item{recruitTime}{The time of recruitment for this patient Id}

\item{event}{\code{Event} object describing the event to be processed}
}
\value{
A value to be inserted into the appropriate cell
of the data frame returned by \code{\link{ProcessEventSimOutput}}
}
\description{
This function performs \code{\link{ProcessEventSimOutput}} for a
single patient and single event
}

