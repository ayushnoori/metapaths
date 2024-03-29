% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get-node-list.R
\name{get_node_list}
\alias{get_node_list}
\title{Get node list.}
\usage{
get_node_list(edge_list)
}
\arguments{
\item{edge_list}{Edge list as a \code{data.table} which must contain the following columns:
\describe{
  \item{\code{Origin}}{IDs of the origin nodes for each edge.}
  \item{\code{Destination}}{IDs of the destination nodes for each edge.}
  \item{\code{OriginType}}{Types of the origin node for each edge.}
  \item{\code{DestinationType}}{Types of the destination node for each edge.}
  \item{\code{EdgeType}}{Types of each edge.}}}
}
\value{
Node list as a \code{data.table} which contains following columns:
\describe{
  \item{\code{Node}}{Node IDs (corresponding to either \code{Origin} or \code{Destination} in the edge list).}
  \item{\code{NodeType}}{Node types (corresponding to either \code{OriginType} or
  \code{DestinationType} in the edge list).}}
}
\description{
Construct the node list from an edge list.
}
