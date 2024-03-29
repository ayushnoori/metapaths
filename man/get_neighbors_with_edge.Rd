% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get-neighbors.R
\name{get_neighbors_with_edge}
\alias{get_neighbors_with_edge}
\title{Get node neighbors of specified type using edge list accounting for edge type.}
\usage{
get_neighbors_with_edge(root, type, edge_list)
}
\arguments{
\item{root}{ID of the root node.}

\item{type}{Desired type of the neighbors.}

\item{edge_list}{Edge list as a \code{data.table} which must contain the following columns:
\describe{
  \item{\code{Origin}}{IDs of the origin nodes for each edge.}
  \item{\code{Destination}}{IDs of the destination nodes for each edge.}
  \item{\code{OriginType}}{Types of the origin node for each edge.}
  \item{\code{DestinationType}}{Types of the destination node for each edge.}
  \item{\code{EdgeType}}{Types of each edge.}}}
}
\value{
Vector of neighbors of a given type.
}
\description{
Retrieve the neighbors of a given node which are of a given type, accounting for edge type.
}
