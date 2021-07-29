#' Get node neighbors of specified type using edge list.
#'
#' Retrieve the neighbors of a given node which are of a given type.
#'
#' @template root-type
#' @template edge-list
#' @return Vector of neighbors of a given type.
#' @export
get_similarity = function(x, y, mp,
                          metric = c("pc", "npc", "dwpc", "pathsim"),
                          edge_list = NULL, neighbor_list = NULL,
                          verbose = TRUE) {

  # get types
  type_x = mp[1]; type_y = mp %>% .[length(.)]

  # check that origin node is of type specified by meta-path
  type_x %>% { if(nlist[node == x, node_bioentity] != .) stop("Origin node must be of type ", ., ".", call. = FALSE)}

  # check that destination node is of type specified by meta-path
  type_y %>% .[length(.)] %>% { if(nlist[node == y, node_bioentity] != .) stop("Destination node must be of type ", ., ".", call. = FALSE)}

  # check that either an edge list OR a neighbor list are provided, but NOT both
  null_e = is.null(edge_list); null_n = is.null(neighbor_list)
  if((null_e + null_n) != 1) stop("Must specify either an edge list or a list of neighbors by type.", call. = FALSE)

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

  # get all paths from destination following specified meta-path
  if(verbose) message("\n>>> Computing Paths from Destination (", y, ")")
  paths_y = traverse_mp(y, mp, reference_list = reference_list,
                        list_type = list_type, verbose = verbose)

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
