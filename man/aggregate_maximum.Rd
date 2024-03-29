% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aggregation-methods.R
\name{aggregate_maximum}
\alias{aggregate_maximum}
\title{Use maximum aggregation.}
\usage{
aggregate_maximum(sim_dt, verbose)
}
\arguments{
\item{sim_dt}{A \code{data.table} containing the collapsed output of multiple \code{get_similarity()$Similarity} calls extracted by \code{extract_sim()}.}

\item{verbose}{Should aggregation method be printed to the console?}
}
\value{
A \code{data.table} with multiple columns, including:
\describe{
  \item{Metric}{The name of the similarity metric used (e.g., \code{"PathSim"}).}
  \item{Method}{The name of the aggregation method used (i.e., \code{"Maximum"}).}
  \item{SetSimilarity}{The aggregate set similarity score.}}
}
\description{
Use the maximum aggregation method to aggregate meta-path based similarity scores.
}
\references{
\cite{Guney, E., Menche, J., Vidal, M. & Barábasi, A.-L. Network-based in silico drug efficacy screening. Nat Commun 7, 10331 (2016).}
}
\seealso{
\code{get_similarity()} for pairwise meta-path based similarity calculation.
}
