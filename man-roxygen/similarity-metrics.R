#' @param x ID of the origin node.
#' @param y ID of the destination node.
#' @param paths_x Paths from the origin node following the meta-path of interest as a \code{data.table}.
#' @param paths_y Paths from the destination node following the meta-path of interest as a \code{data.table}.
#' @param reference_list Either an edge list as a \code{data.table} which must contain the columns \code{Origin},
#' \code{Destination}, \code{OriginType}, and \code{DestinationType}, or a neighbor reference object constructed
#' by \code{get_neighbor_list()}.
#' @param list_type If an edge list is provided, specify \code{"edge"}. If a neighbor list is provided, specify \code{"neighbor"}.
#' @param verbose Should the intermediate calculations be printed to the console?
#' @family similarity
#' @seealso \code{get_neighbor_list()} for neighbor reference object construction.
