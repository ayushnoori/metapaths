#' @param reference_list Either an edge list as a \code{data.table} or a neighbor reference object constructed
#' by \code{get_neighbor_list()}. If an edge list is provided, the \code{data.table} must contain the following columns:
#' \describe{
#'   \item{\code{Origin}}{IDs of the origin nodes for each edge.}
#'   \item{\code{Destination}}{IDs of the destination nodes for each edge.}
#'   \item{\code{OriginType}}{Types of the origin node for each edge.}
#'   \item{\code{DestinationType}}{Types of the destination node for each edge.}
#'   \item{\code{EdgeType}}{Types of each edge.}}
#' @param list_type If an edge list is provided, specify \code{"edge"}. If a neighbor list is provided, specify \code{"neighbor"}.
#' @seealso \code{get_neighbor_list()} for neighbor reference object construction.
