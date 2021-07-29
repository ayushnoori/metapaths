#' @include get-neighbors.R
NULL

#' Construct neighbors reference object.
#'
#' For a given edge list, construct a nested list (i.e., dictionary of dictionaries in Python syntax)
#' of the neighbors of each node in the edge list stratified by type. This reference object can be
#' used instead of the edge list in subsequent \code{metapaths} functions, since searching the edge list
#' to recompute neighbors each time would have \code{O(n)} complexity, while searching a pre-computed neighbors
#' dictionary requires a fixed amount of time.
#'
#' @template edge-list
#' @param file_path File path to save final \code{data.table} reference object. If no path is specified, the
#' reference object is not saved.
#' @param intermediate_file_path File path to save intermediate list of lists reference object. If no path is
#' specified, the intermediate reference object is not saved.
#' @param verbose Should node types and total node count be printed to the console?
#' @return Neighbors reference object as a \code{data.table}
#' @examples
#' mtcars_neighbor_list = get_neighbor_list(mtcars_edge_list, verbose = T)
#' head(mtcars_neighbor_list)
#' @export
get_neighbor_list = function(edge_list, file_path = NULL, intermediate_file_path = NULL, verbose = F) {

  # get edge list types
  edge_list_types = node_types(edge_list, verbose)
  if(verbose) message()
  edge_list_nodes = all_nodes(edge_list, verbose)

  # create neighbors object
  neighbor_list = map(edge_list_nodes, ~get_neighbors_type(.x, edge_list_types, edge_list))

  # save intermediate if needed
  if(!is.null(intermediate_file_path)) saveRDS(neighbor_list, file = intermediate_file_path)

  # convert to data table
  neighbors_DT = map(neighbor_list, ~data.table(t(.x))) %>%
    rbindlist(fill = T, idcol = "Node") %>%
    setkey(Node)

  # save file if needed
  if(!is.null(file_path)) saveRDS(neighbors_DT, file = file_path)

  # return neighbors data table
  return(neighbors_DT)

}
