#' @include search-degrees.R map-neighbors.R
NULL

#' Compute PathSim similarity.
#'
#' Use the PathSim metric to compute a meta-path based similarity score. Please note that PathSim can ONLY
#' be used for symmetric meta-paths.
#'
#' @template similarity-metrics
#' @return A list with two elements:
#' \describe{
#'   \item{Metric}{The name of the similarity metric (i.e., \code{"PathSim"}).}
#'   \item{Similarity}{The PathSim similarity score.}}
#' @references \cite{Sun, Y., Han, J., Yan, X., Yu, P. S. & Wu, T. PathSim: meta path-based top-K similarity search in heterogeneous information networks. Proc. VLDB Endow. 4, 992–1003 (2011).}
#' @export
get_pathsim = function(x, y, paths_x, paths_y, reference_list = NULL,
                       list_type = NULL, verbose = TRUE) {

  x_y = sum(paths_x[[ncol(paths_x)]] == y)
  x_x = sum(paths_x[[ncol(paths_x)]] == x)
  y_y = sum(paths_y[[ncol(paths_y)]] == y)

  if(x_y == 0 | (x_x + y_y) == 0) pathsim = 0 else pathsim = (2*x_y)/(x_x + y_y)

  if(verbose) {
    message("Similarity Metric: PathSim")
    message("X -> Y Paths: ", x_y)
    message("X -> X Paths: ", x_x)
    message("Y -> Y Paths: ", y_y)
    message("Similarity: ", pathsim)
  }

  return(list(Metric = "PathSim", Similarity = pathsim))

}


#' Compute path count similarity.
#'
#' Use the path count metric to compute a meta-path based similarity score.
#'
#' @template similarity-metrics
#' @return A list with two elements:
#' \describe{
#'   \item{Metric}{The name of the similarity metric (i.e., \code{"Path Count"}).}
#'   \item{Similarity}{The path count similarity score.}}
#' @references \cite{Himmelstein, D. S. & Baranzini, S. E. Heterogeneous Network Edge Prediction: A Data Integration Approach to Prioritize Disease-Associated Genes. PLOS Computational Biology 11, e1004259 (2015).}
#' @export
get_pc = function(x, y, paths_x, paths_y = NULL, reference_list = NULL,
                  list_type = NULL, verbose = TRUE) {

  x_y = sum(paths_x[[ncol(paths_x)]] == y)
  pc = x_y

  if(verbose) {
    message("Similarity Metric: Path Count")
    message("X -> Y Paths: ", x_y)
    message("Similarity: ", pc)
  }

  return(list(Metric = "Path Count", Similarity = pc))

}


#' Compute normalized path count similarity.
#'
#' Use the normalized path count metric to compute a meta-path based similarity score.
#'
#' @template similarity-metrics
#' @return A list with two elements:
#' \describe{
#'   \item{Metric}{The name of the similarity metric (i.e., \code{"Normalized Path Count"}).}
#'   \item{Similarity}{The normalized path count similarity score.}}
#' @references \cite{Himmelstein, D. S. & Baranzini, S. E. Heterogeneous Network Edge Prediction: A Data Integration Approach to Prioritize Disease-Associated Genes. PLOS Computational Biology 11, e1004259 (2015).}
#' @export
get_npc = function(x, y, paths_x, paths_y, reference_list = NULL,
                   list_type = NULL, verbose = TRUE) {

  x_y = sum(paths_x[[ncol(paths_x)]] == y)
  x_typey = nrow(paths_x)
  y_typex = nrow(paths_y)
  if(x_y == 0 | (x_typey + y_typex) == 0) npc = 0 else  {
    npc = x_y/(x_typey + y_typex) }

  if(verbose) {
    message("Similarity Metric: Normalized Path Count")
    message("X -> Y Paths: ", x_y)
    message("X -> Type Y Paths: ", x_typey)
    message("Y -> Type X Paths: ", y_typex)
    message("Similarity: ", npc)
  }

  return(list(Metric = "Normalized Path Count", Similarity = npc))

}


#' Compute degree-weighted path count similarity.
#'
#' Use the degree-weighted path count metric to compute a meta-path based similarity score. Node that, in this implementation,
#' type-specific degrees are used (except for the last step of the meta-path).
#'
#' @template similarity-metrics
#' @return A list with two elements:
#' \describe{
#'   \item{Metric}{The name of the similarity metric (i.e., \code{"Degree-Weighted Path Count"}).}
#'   \item{Similarity}{The degree-weighted path count similarity score.}}
#' @references \cite{Himmelstein, D. S. & Baranzini, S. E. Heterogeneous Network Edge Prediction: A Data Integration Approach to Prioritize Disease-Associated Genes. PLOS Computational Biology 11, e1004259 (2015).}
#' @export
get_dwpc = function(x, y, paths_x, paths_y = NULL, reference_list,
                    list_type = c("edge", "neighbor"), verbose = TRUE, w = 0.4) {

  # get paths from origin to destination
  x_y_paths = paths_x[paths_x[[ncol(paths_x)]] == y, ]
  x_y = nrow(x_y_paths)

  # get vector of next path in meta-path, last is NA (so total degree is computed)
  next_type = c(clean_mp(colnames(x_y_paths)), NA)

  # compute type-specific degrees
  map_search = function(node_list, type_next) { map_dbl(node_list, ~search_degrees(.x, type_next, reference_list, list_type)) }
  x_y_degrees = map2_dfr(x_y_paths, seq_along(x_y_paths), ~map_search(.x, next_type[.y + 1])) %>% setDT()

  # for each path, compute path degree product (PDP)
  pdp = pmap_dbl(x_y_degrees, ~prod(c(...)^-w))
  if(x_y == 0) dwpc = 0 else dwpc = sum(pdp)

  if(verbose) {
    if(length(pdp) > 1) sd_msg = paste0(" [", sd(pdp), "]") else sd_msg = ""
    message("Similarity Metric: Degree-Weighted Path Count")
    message("X -> Y Paths: ", x_y)
    message("Damping Exponent: ", w)
    message("PDP (Mean/SD): ", mean(pdp), sd_msg)
    message("Similarity: ", dwpc)
  }

  return(list(Metric = "Degree-Weighted Path Count", Similarity = dwpc))

}


#' Retrieve similarity score metric.
#'
#' For a desired meta-path based similarity metric, retrieve the function which computes the appropriate similarity score, then compute similarity.
#'
#' @param metric_name Readable description of the desired similarity score:
#' \describe{
#'   \item{Path Count}{Use \code{"pc"}.}
#'   \item{Normalized Path Count}{Use \code{"npc"}.}
#'   \item{Degree-Weighted Path Count}{Use \code{"dwpc"}.}
#'   \item{PathSim}{Use \code{"pathsim"}.}}
#' @param get_verbose Should output be printed to console?
#' @param ... Other arguments to pass to appropriate meta-path similarity function.
#' @return A list with two elements:
#' \describe{
#'   \item{Metric}{The human readable name of the similarity metric.}
#'   \item{Similarity}{The appropriate similarity score.}}
#' @seealso See the following functions to compute various similarity scores:
#' \describe{
#'   \item{Path Count}{\code{get_pc()}}
#'   \item{Normalized Path Count}{\code{get_npc()}}
#'   \item{Degree-Weighted Path Count}{\code{get_dwpc()}}
#'   \item{PathSim}{\code{get_pathsim()}}}
#' @export
get_similarity_function = function(metric_name, get_verbose, ...) {

  # specify allowed values (i.e., defined functions)
  if(metric_name %in% c("pc", "npc", "dwpc", "pathsim")) {
    if(get_verbose) message()
    return(get(paste0("get_", metric_name))(...))
  } else {
    if(get_verbose) message("\nSimilarity Metric: N/A")
    return(list(Metric = "Not Available", Similarity = NA))
  }

}
