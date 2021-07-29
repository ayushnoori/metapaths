#' Get node types.
#'
#' Retrieve the node types from an edge list.
#'
#' @template edge-list
#' @param verbose Should node types be printed to the console?
#' @return List of node types.
#' @export
node_types = function(edge_list, verbose = F) {
  ntypes = edge_list[, unique(c(OriginType, DestinationType))] %>%
    purrr::set_names(.)
  if(verbose) message("Node Types:\n- ", paste(ntypes, collapse = "\n- "))
  return(ntypes)
}
