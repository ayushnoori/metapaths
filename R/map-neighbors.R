#' @include get-neighbors.R search-neighbors.R
NULL

#' Clean type name along meta-path.
#'
#' For a given meta-path and root node, when the meta-ath \code{data.table} is constructed, steps along
#' the meta-path with the same type (e.g., \code{Author-Date-Author}) are assigned the same column name.
#' Then, \code{make.unique()} is run to deduplicate column names by appending \code{.1}, \code{.2}, etc.
#' When querying the edge list (or neighbor list), the original (non-unique) type name is required.
#' Hence, this function strips the unique suffix from the type name (e.g., removes \code{.1}, \code{.2}, etc.).
#'
#' @param type Name of the type along any meta-path of interest.
#' @return Cleaned type name.
#' @export
clean_mp = function(type) {
  return(gsub("[^A-Za-z]", "", type))
}


#' Step along meta-path to retrieve neighbors.
#'
#' For a given set of root nodes, step along a given meta-path to find their neighbors
#' using either \code{get_neighbors()} (for edge lists) or \code{search_neighbors()} (for
#' neighbor lists).
#'
#' @param roots_subset Paths from a specific root node as a \code{data.table}.
#' @param current_mp Label of the current type along the meta-path.
#' @param next_mp Label of the next type along the meta-path.
#' @template reference-list
#' @return Neighbors of the root nodes of a specified type (along the meta-path) as a \code{data.table}.
#' @export
map_neighbors = function(roots_subset, current_mp, next_mp,
                         reference_list, list_type = c("edge", "neighbor")) {

  # if edge list is specified
  if(list_type == "edge") neighbors = get_neighbors(roots_subset[, get(current_mp)], clean_mp(next_mp), reference_list)

  # if neighbor list is specified
  if(list_type == "neighbor") neighbors = search_neighbors(roots_subset[, get(current_mp)], clean_mp(next_mp), reference_list)

  # get neighbors
  roots_subset %>%
    .[rep(1, length(neighbors)), ] %>%
    .[, eval(next_mp) := neighbors] %>%
    return()

}


#' Step along meta-path to get all conforming paths from root node.
#'
#' Recursive function to traverse the meta-path until is is expounded. The variable \code{mp} is a list of node types (i.e., a meta-path),
#' which is shortened on each call of \code{traverse_mp()}.
#'
#' @param roots ID of the root node on the first iteration, otherwise, \code{data.table} of growing paths.
#' @param mp List of node types (i.e., a meta-path).
#' @param step Counter for recursive function.
#' @template reference-list
#' @param verbose Should intermediate calculations be printed to console?
#' @return List of all meta-paths from the root node following a specific meta-path, as a \code{data.table}.
#' @export
traverse_mp = function(roots, mp, step = 1, reference_list,
                       list_type = c("edge", "neighbor"), verbose = TRUE) {

  # if at origin node
  if(step == 1) {
    mp = make.unique(mp)
    roots = c(roots, rep(NA, length(mp)-1)) %>%
      matrix(nrow = 1) %>%
      data.table() %>%
      setNames(mp)
    if(verbose) message("Step ", step, " (", mp[1], "): ", nrow(roots), " Path")
  }

  # if at destination node
  if(length(mp) == 1) return(roots)

  # get neighbors of type
  roots = pmap_dfr(roots, ~map_neighbors(data.table(...), mp[1], mp[2],
                                         reference_list, list_type))

  # remove nodes which have no neighbors of specified type
  rem = map_lgl(roots[, get(mp[2])], is.na)
  if((sum(rem) > 0) & verbose) message("-  ", sum(rem), " Path(s) Terminated")
  roots = roots[!rem, ]

  # if no paths remain
  if(nrow(roots) == 0) {
    if(verbose) message("No paths exist following specified meta-path.")
    return(roots)
  }

  # print message
  if(verbose) message("Step ", step + 1, " (", clean_mp(mp[2]), "): ",
                      nrow(roots), " Paths")

  # recursive step
  return(traverse_mp(roots, tail(mp, -1), step + 1, reference_list,
                     list_type, verbose = verbose))

}
