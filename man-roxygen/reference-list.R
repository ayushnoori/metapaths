#' @param reference_list Either an edge list as a \code{data.table} which must contain the columns \code{Origin},
#' \code{Destination}, \code{OriginType}, and \code{DestinationType}, or a neighbor reference object constructed
#' by \code{get_neighbor_list()}.
#' @param list_type If an edge list is provided, specify \code{"edge"}. If a neighbor list is provided, specify \code{"neighbor"}.
#' @seealso \code{get_neighbor_list()} for neighbor reference object construction.
