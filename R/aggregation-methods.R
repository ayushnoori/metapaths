#' Use shortest aggregation.
#'
#' Use the shortest aggregation method to aggregate meta-path based similarity scores.
#'
#' @param sim_dt A \code{data.table} containing the collapsed output of multiple \code{get_similarity()$Similarity} calls extracted by \code{extract_sim()}.
#' @param verbose Should aggregation method be printed to the console?
#' @return A \code{data.table} with multiple columns, including:
#' \describe{
#'   \item{Metric}{The name of the similarity metric used (e.g., \code{"PathSim"}).}
#'   \item{Method}{The name of the aggregation method used (i.e., \code{"Shortest"}).}
#'   \item{SetSimilarity}{The aggregate set similarity score.}}
#' @references \cite{Guney, E., Menche, J., Vidal, M. & Barábasi, A.-L. Network-based in silico drug efficacy screening. Nat Commun 7, 10331 (2016).}
#' @seealso \code{get_similarity()} for pairwise meta-path based similarity calculation.
#' @export
aggregate_shortest = function(sim_dt, verbose) {
  agg = sim_dt[, min(Similarity), by = c("Metric", "Origin")] %>%
    .[, mean(V1), by = c("Metric")] %>%
    .[, Method := "Shortest"] %>%
    setnames("V1", "SetSimilarity") %>%
    setcolorder(c("Metric", "Method", "SetSimilarity"))
  if(verbose) message("Aggregation Method: Shortest")
  return(agg)
}


#' Retrieve aggregation method.
#'
#' For a desired aggregation method, retrieve the function which aggregates the meta-path based similarity scores. This function is a wrapper around \code{get_similarity()}.
#'
#' @param method_name Readable description of the desired aggregation method:
#' \describe{
#'   \item{Shortest}{Use \code{"shortest"}.}}
#' @param get_verbose Should output be printed to console?
#' @param ... Other arguments to pass to appropriate meta-path similarity function.
#' @return The output of the desired aggregation method.
#' @seealso See the following aggregation functions:
#' \describe{
#'   \item{Shortest}{\code{aggregate_shortest()}}}
#' @export
get_aggregation_function = function(method_name, get_verbose, ...) {

  # specify allowed values (i.e., defined functions)
  if(method_name %in% c("shortest")) {
    if(get_verbose) message()
    return(get(paste0("aggregate_", method_name))(...))
  } else {
    if(get_verbose) message("\nAggregation Method: N/A")
    return(NA)
  }

}
