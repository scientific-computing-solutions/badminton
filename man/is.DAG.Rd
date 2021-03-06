% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/progressionGraph.R
\name{is.DAG}
\alias{is.DAG}
\title{Checks whether a graphNEL object is a DAG}
\usage{
is.DAG(graph)
}
\arguments{
\item{graph}{An object of type \code{graphNEL}}
}
\value{
True if DAG, false otherwise
}
\description{
Returns true if an graphNEL object is a directed acyclic graph. Specifically if
edgemode(graph) == "directed" and the graph contains no cycles.
}

