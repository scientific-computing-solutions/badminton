% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/event.R
\name{ValidateAgainstProgressionGraph.EventSet}
\alias{ValidateAgainstProgressionGraph.EventSet}
\title{Sanity Check an \code{EventSet} with respect to a \code{ProgressionGraph}}
\usage{
ValidateAgainstProgressionGraph.EventSet(object, progressionGraph)
}
\arguments{
\item{object}{An \code{EventSet} object}

\item{progressionGraph}{A \code{progressionGraph} object}
}
\value{
True if all nodes and edges described by events in \code{object}
are found in the \code{ProgressionGraph} DAG. If this function returns True then
\code{ProcessEventSimOutput} can be called with an output from a simulation
which \code{ProgressionGraph} is the underlying DAG and the EventSet is \code{object}.

There may be occasions where it still makes sense for \code{ProcessEventSimOutput} to
be called even if this function returns FALSE
}
\description{
Sanity Check an \code{EventSet} with respect to a \code{ProgressionGraph}
}

