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
#'   \item{Method}{The name of the aggregation method used.}
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
#' @return After printing to the console if \code{verbose} is \code{TRUE}, \code{res} is returned as is.
#' @export
map_log = function(res, verbose) {
  if(verbose) message("\n", rep("_", 80), "\n")
  return(res)
}

#' Compute meta-path based similarity scores between two sets of nodes.
#'
#' Given a specified meta-path, similarity metric(s), and aggregation method(s), compute the meta-path based similarity score between two nodes.
#'
#' @param set1 IDs of node set #1 as a vector.
#' @param set2 IDs of node set #2 as a vector.
#' @template mp-metric
#' @template node-list
#' @template edge-list
#' @template neighbor-list
#' @param check Should type checking be performed? Default is \code{TRUE}.
#' @param verbose Should the intermediate calculations be printed to the console?
#' @return A list with six elements:
#' \describe{
#'   \item{Set1}{IDs of node set #1 provided (i.e., \code{set1}).}
#'   \item{Set2}{ID of node set #2 provided (i.e., \code{set2}).}
#'   \item{Comparisons}{List of pairwise comparisons.}
#'   \item{Details}{Intermediate computations produced by \code{get_similarity()}.}
#'   \item{Similarities}{Pairwise similarity scores.}
#'   \item{SetSimilarity}{Aggregate set similarity score(s).}}
#' @seealso \code{get_neighbor_list()} for neighbor reference object construction,
#' \code{get_similarity_function()} for similarity metrics, and
#' \code{get_aggregation_function()} for aggregation methods. This function is a
#' wrapper around \code{get_similarity()}.
#' @export
compare_sets = function(set1, set2, mp,
                        metric = c("pc", "pathsim", "npc", "dwpc"),
                        method = c("maximum"), node_list,
                        edge_list = NULL, neighbor_list = NULL,
                        check = TRUE, verbose = TRUE) {

  # first, make unique if not already
  set1 = unique(set1); set2 = unique(set2)

  # create pairwise combinations
  pairs = data.table(Set1 = rep(set1, each = length(set2)),
                     Set2 = rep(set2, length(set1)))

  # compute similarities
  pair_sims = pmap(pairs, ~map_log(get_similarity(..1, ..2, mp, metric, node_list, edge_list, neighbor_list, check, verbose = verbose), verbose))
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


#' Compute meta-path based similarity scores between two sets of nodes using various meta-paths.
#'
#' Given a series of specified meta-path(s), similarity metric(s), and aggregation method(s), compute the meta-path based similarity score between two nodes.
#'
#' @param set1 IDs of node set #1 as a vector.
#' @param set2 IDs of node set #2 as a vector.
#' @template mps-metric
#' @template method
#' @template node-list
#' @template edge-list
#' @template neighbor-list
#' @param check Should type checking be performed? Default is \code{TRUE}.
#' @param verbose Should the intermediate calculations be printed to the console?
#' @return A list with six elements:
#' \describe{
#'   \item{Set1}{IDs of node set #1 provided (i.e., \code{set1}).}
#'   \item{Set2}{ID of node set #2 provided (i.e., \code{set2}).}
#'   \item{Details}{Intermediate computations produced by \code{get_similarity()} and \code{compare_sets()}.}
#'   \item{SetSimilarity}{Aggregate set similarity score(s).}}
#' @seealso \code{get_neighbor_list()} for neighbor reference object construction,
#' \code{get_similarity_function()} for similarity metrics, and
#' \code{get_aggregation_function()} for aggregation methods. This function is a
#' wrapper around \code{compare_sets()}, which in turn is a wrapper around \code{get_similarity()}.
#' @export
compare_mps = function(set1, set2, mps,
                       metric = c("pc", "pathsim", "npc", "dwpc"),
                       method = c("maximum"), node_list,
                       edge_list = NULL, neighbor_list = NULL,
                       check = TRUE, verbose = TRUE) {

  # compare meta-paths
  comp_mps = map(mps, ~compare_sets(set1, set2, mp = .x, metric, method, node_list,
                                    edge_list, neighbor_list, check, verbose))
  set_similarity = map(comp_mps, ~.$SetSimilarity) %>% rbindlist()

  # return similarity
  return(list(Set1 = set1, Set2 = set2,
              Details = comp_mps, SetSimilarity = set_similarity))

}
