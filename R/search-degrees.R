#' @include search-neighbors.R node-types.R get-neighbors.R
NULL

#' Get degree count by type.
#'
#' Compute degree count stratified by node type using either an edge list or neighbor reference object computed by \code{get_neighbor_list()}.
#'
#' @template root-type
#' @template reference-list
#' @return Degree count stratified by type.
#' @export
search_degrees = function(root, type = NA, reference_list, list_type = c("edge", "neighbor")) {

  # if working with neighbor list
  if(list_type == "neighbor") {

    # if type is NA, return total degree in network
    # otherwise, return number of neighbors of that type
    if(is.na(type)) {

      unlist(reference_list[Node == root, !c("Node")]) %>% {.[!is.na(.)]} %>%
        length() %>% return()

    } else {
      return(length(search_neighbors(root, type, reference_list)))
    }

    # identical to above, except for edge list rather than neighbor list
  } else if (list_type == "edge") {

    if(is.na(type)) {

      edge_list_types = node_types(reference_list, verbose = F)
      neighbor_list = get_neighbors_type(root, edge_list_types, reference_list)
      unlist(neighbor_list) %>% {.[!is.na(.)]} %>%
        length() %>% return()

    } else {
      return(length(get_neighbors(root, type, reference_list)))
    }

  } else { stop("Must provide either an edge list or a list of neighbors by type.", call. = FALSE) }

}
