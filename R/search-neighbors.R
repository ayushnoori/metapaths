#' Get node neighbors of specified type using neighbors reference object.
#'
#' Retrieve the neighbors of a given node which are of a given type using the neighbors reference object.
#'
#' @template root-type
#' @template neighbor-list
#' @return Vector of neighbors of a given type.
#' @seealso \code{get_neighbor_list()} for neighbor reference object construction.
#' @export
search_neighbors = function(root, type, neighbor_list)  {
  neighbor_list[Node == root, get(type)][[1]]
}
