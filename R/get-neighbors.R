#' Get node neighbors of specified type using edge list.
#'
#' Retrieve the neighbors of a given node which are of a given type.
#'
#' @template root-type
#' @template edge-list
#' @return Vector of neighbors of a given type.
#' @export
get_neighbors = function(root, type, edge_list) {

  # get neighbors for origin and destination nodes
  neighbors_destination = edge_list[Origin == root & DestinationType == type]
  neighbors_origin = edge_list[Destination == root & OriginType == type]

  neighbors = unique(c(neighbors_origin[, Origin],
                       neighbors_destination[, Destination]))
  if(length(neighbors) == 0) return(NA) else return(neighbors)

}


#' Get all node neighbors by type.
#'
#' Retrieve all neighbors of a given node, stratified by specific node types.
#'
#' @template root
#' @param edge_list_types Specified node types to stratify by.
#' @template edge-list
#' @return Nested list of neighbors grouped by type.
#' @export
get_neighbors_type = function(root, edge_list_types, edge_list) {
  map(edge_list_types, ~get_neighbors(root, .x, edge_list))
}


#' Get node neighbors of specified type using edge list accounting for edge type.
#'
#' Retrieve the neighbors of a given node which are of a given type, accounting for edge type.
#'
#' @template root-type
#' @template edge-list
#' @return Vector of neighbors of a given type.
#' @export
get_neighbors_with_edge = function(root, type, edge_list) {

  # get neighbors for origin and destination nodes
  neighbors_destination = edge_list[Origin == root & DestinationType == type]
  neighbors_origin = edge_list[Destination == root & OriginType == type]

  neighbors = unique(c(neighbors_origin[, paste(Origin, EdgeType, sep = "_")],
                       neighbors_destination[, paste(Destination, EdgeType, sep = "_")]))
  if(length(neighbors) == 0) return(NA) else return(neighbors)

}


#' Get all node neighbors by type accounting for edge type.
#'
#' Retrieve all neighbors of a given node, stratified by specific node types and accounting for edge type.
#'
#' @template root
#' @param edge_list_types Specified node types to stratify by.
#' @template edge-list
#' @return Nested list of neighbors grouped by type.
get_neighbors_type_with_edge = function(root, edge_list_types, edge_list) {
  map(edge_list_types, ~get_neighbors_with_edge(root, .x, edge_list))
}
