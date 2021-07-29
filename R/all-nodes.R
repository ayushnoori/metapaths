#' Get all nodes.
#'
#' Retrieve a list of all unique nodes from an edge list.
#'
#' @template edge-list
#' @param verbose Should total node count be printed to the console?
#' @return List of unique nodes.
#' @export
all_nodes = function(edge_list, verbose = F) {
  anodes = edge_list[, unique(c(Origin, Destination))] %>%
    purrr::set_names(.)
  if(verbose) message("Total Node Count: ", length(anodes))
  return(anodes)
}
