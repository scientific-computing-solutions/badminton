% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/progressionGraph.R
\name{AddNode.ProgressionGraph}
\alias{AddNode.ProgressionGraph}
\title{Adds nodes to an object of type \code{ProgressionGraph}}
\usage{
AddNode.ProgressionGraph(object, nodeNames, visitSchedule = NULL,
  isVisitNode = FALSE)
}
\arguments{
\item{object}{the \code{ProgressionGraph} to add new nodes}

\item{nodeNames}{a vector of strings to be added as nodes in the graph}

\item{visitSchedule}{Either a \code{VisitSchedule} object which is the visit schedule to
be used for the new nodes to be added or \code{NULL} if no visit schedule is being used (or visit Schedule is
to be added later)}

\item{isVisitNode}{A logical value. Is the new node a `visit' node, i.e. a node for which the transition into it is unknown
until the next visit}
}
\value{
a new \code{ProgressionGraph} with the new nodes added
}
\description{
Adds nodes to an object of type \code{ProgressionGraph}, will
throw an exception if attempting to add a new node with the same
name as an existing node
}

