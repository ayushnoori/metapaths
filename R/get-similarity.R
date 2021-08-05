#' @include map-neighbors.R similarity-metrics.R
NULL

#' Compute meta-path based similarity score between two nodes.
#'
#' Given a specified meta-path and similarity metric(s), compute the meta-path based similarity score between two nodes.
#'
#' @param x ID of the origin node.
#' @param y ID of the destination node.
#' @template mp-metric
#' @template node-list
#' @template edge-list
#' @template neighbor-list
#' @param check Should type checking be performed? Default is \code{TRUE}.
#' @param verbose Should the intermediate calculations be printed to the console?
#' @return A list with six elements:
#' \describe{
#'   \item{Origin}{ID of the origin node provided (i.e., \code{x}).}
#'   \item{Destination}{ID of the destination node provided (i.e., \code{y}).}
#'   \item{MP}{Meta-path provided (i.e., \code{mp}).}
#'   \item{OriginPaths}{Paths following the provided meta-path from the origin node (i.e., \code{x}) to all nodes of the
#'   same type as the destination node (i.e., \code{y}) as a \code{data.table}.}
#'   \item{DestinationPaths}{Paths following the REVERSE of the provided meta-path from the destination node (i.e., \code{y}) to all nodes of the
#'   same type as the origin node (i.e., \code{x}) as a \code{data.table}.}
#'   \item{Similarity}{Computed meta-path based similarity scores by metric as a \code{data.table}.}}
#' @seealso \code{get_neighbor_list()} for neighbor reference object construction and \code{get_similarity_function()} for similarity metrics.
#' @export
get_similarity = function(x, y, mp,
                          metric = c("pc", "npc", "dwpc", "pathsim"),
                          node_list, edge_list = NULL, neighbor_list = NULL,
                          check = TRUE, verbose = TRUE) {

  # unless type checking has been disabled
  if(check) {

    # get types
    type_x = mp[1]; type_y = mp %>% .[length(.)]

    # check that origin node is of type specified by meta-path
    type_x %>% { if(node_list[Node == x, NodeType] != .) stop("Origin node must be of type ", ., ".", call. = FALSE, )}

    # check that destination node is of type specified by meta-path
    type_y %>% .[length(.)] %>% { if(node_list[Node == y, NodeType] != .) stop("Destination node must be of type ", ., ".", call. = FALSE, )}

  }

  # check that either an edge list OR a neighbor list are provided, but NOT both
  null_e = is.null(edge_list); null_n = is.null(neighbor_list)
  if((null_e + null_n) != 1) stop("Must specify either an edge list or a list of neighbors by type.", call. = FALSE, )

  # if edge list is specified
  if(!null_e) {
    reference_list = edge_list; list_type = "edge"
    if(verbose) warning("An edge list has been provided rather than a neighbor list by node type. At scale, this may increase computational time.\n", call. = FALSE, immediate. = TRUE)
  }

  # if neighbor list is specified
  if(!null_n) { reference_list = neighbor_list; list_type = "neighbor" }

  # get all paths from origin following specified meta-path
  if(verbose) message(">>> Computing Paths from Origin (", x, ")")
  paths_x = traverse_mp(x, mp, reference_list = reference_list,
                        list_type = list_type, verbose = verbose)

  # only PathSim and NPC require paths_y to be computed
  if(sum(c("pathsim", "npc") %in% metric) > 0) {
    # get all paths from destination following REVERSE of specified meta-path
    if(verbose) message("\n>>> Computing Paths from Destination (", y, ")")
    paths_y = traverse_mp(y, rev(mp), reference_list = reference_list,
                          list_type = list_type, verbose = verbose)
  } else { paths_y = NULL }

  # compute similarity
  if(verbose) message("\n>>> Computing Similarity")
  mp_similarity = map(metric, ~get_similarity_function(metric_name = .x, get_verbose = verbose, x, y, paths_x, paths_y, reference_list, list_type, verbose)) %>% rbindlist()

  return(list(
    Origin = x,
    Destination = y,
    MP = mp,
    OriginPaths = paths_x,
    DestinationPaths = paths_y,
    Similarity = mp_similarity
  ))

}
