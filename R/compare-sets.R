#' @include get-similarity.R aggregation-methods.R
NULL

#' Extract similarity scores from pairwise comparisons.
#'
#' During aggregation of meta-path based similarity, extract similarity scores from pairwise aggregations.
#'
#' @param x A \code{data.table} containing the output of a \code{get_similarity()$Similarity} call.
#' @param verbose Should aggregation method be printed to the console?
#' @return A data.table with multiple columns elements:
#' \describe{
#'   \item{Origin}{The ID of the origin node.}
#'   \item{Destination}{The ID of the destination node.}
#'   \item{Metric}{The name of the similarity metric used (e.g., \code{"PathSim"}).}
#'   \item{Method}{The name of the aggregation method used.}}
#'   \item{Similarity}{The pairwise meta-path based similarity score.}}
#' @seealso \code{get_similarity()} for pairwise meta-path based similarity calculation.
#' @export
extract_sim = function(x) {
  x$Similarity %>%
    .[, Origin := x$Origin] %>%
    .[, Destination := x$Destination] %>%
    setcolorder(c("Origin", "Destination")) %>%
    return()
}

#' Add dividers between analysis logs.
#'
#' Print dividers to the console between analysis logs when \code{verbose} is \code{TRUE}.
#'
#' @param res An intermediate value of an iterable function (e.g., \code{purrr::map}).
#' @param verbose Should dividers be printed to the console?
#' @return \code{res} is returned as is.
#' @export
map_log = function(res, verbose) {
  if(verbose) message("\n", rep("_", 80), "\n")
  return(res)
}

#' Compute meta-path based similarity scores between two nodes.
#'
#' Retrieve the neighbors of a given node which are of a given type.
#'
#' @template get-similarity
#' @template node-list
#' @template neighbor-list
#' @template reference-list
#' @param verbose Should the intermediate calculations be printed to the console?
#' @return A list with six elements:
#' \describe{
#'   \item{Origin}{ID of the origin node provided (i.e., \code{x}).}
#'   \item{Destination}{ID of the destination node provided (i.e., \code{y}).}
#'   \item{MP}{Meta-path provided (i.e., \code{mp}).}
#'   \item{OriginPaths}{Paths following the provided meta-path from the origin node (i.e., \code{x}) to all nodes of the
#'   same type as the destination node (i.e., \code{y}) as a \code{data.table}.}
#'   \item{OriginPaths}{Paths following the REVERSE of the provided meta-path from the destination node (i.e., \code{y}) to all nodes of the
#'   same type as the origin node (i.e., \code{x}) as a \code{data.table}.}
#'   \item{Similarity}{Computed meta-path based similarity scores by metric as a \code{data.table}.}}
#' @export
compare_sets = function(set1, set2, mp,
                        metric = c("pc", "pathsim", "npc", "dwpc"),
                        method = c("shortest"), node_list,
                        edge_list = NULL, neighbor_list = NULL,
                        verbose = TRUE) {

  # first, make unique if not already
  set1 = unique(set1); set2 = unique(set2)

  # create pairwise combinations
  pairs = data.table(Set1 = rep(set1, each = length(set2)),
                     Set2 = rep(set2, length(set1)))

  # compute similarities
  pair_sims = pmap(pairs, ~map_log(get_similarity(..1, ..2, mp, metric, node_list, edge_list, neighbor_list, verbose = verbose), verbose))
  names(pair_sims) = paste(pairs$Set1, pairs$Set2, sep = "-")

  # extract and label similarity scores for each comparison
  sim_dt = rbindlist(map(pair_sims, extract_sim))

  # aggregate similarity
  if(verbose) message("\n>>> Aggregating Similarity")
  set_similarity = map(method, ~get_aggregation_function(method_name = .x, get_verbose = verbose, sim_dt, verbose)) %>% rbindlist() %>%
    .[, MP := paste(mp, collapse = "-")] %>%
    setcolorder("MP")

  return(list(Set1 = set1, Set2 = set2,
              Comparisons = pairs, Details = pair_sims,
              Similarities = sim_dt, SetSimilarity = set_similarity))

}
