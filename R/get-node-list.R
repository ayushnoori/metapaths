#' Get node list.
#'
#' Construct the node list from an edge list.
#'
#' @template edge-list
#' @return Node list as a \code{data.table} which contains following columns:
#' \describe{
#'   \item{\code{Node}}{Node IDs (corresponding to either \code{Origin} or \code{Destination} in the edge list).}
#'   \item{\code{NodeType}}{Node types (corresponding to either \code{OriginType} or
#'   \code{DestinationType} in the edge list).}}
#' @export
get_node_list = function(edge_list) {

  node_list = edge_list[, .(Origin, OriginType)] %>%
    rbind(edge_list[, .(Destination, DestinationType)], use.names = F) %>%
    unique() %>%
    setnames(c("Node", "NodeType"))

  return(node_list)

}
