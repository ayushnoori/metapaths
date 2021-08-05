#' @param x ID of the origin node.
#' @param y ID of the destination node.
#' @param mp Meta-path as a vector of node types (e.g., \code{c("TypeA", "TypeB", "TypeC")}).
#' @param metric A vector of permissible similarity metrics (e.g., \code{c("npc", "dwpc")}), see defined metrics below:
#' \describe{
#'   \item{Path Count}{Use \code{"pc"}.}
#'   \item{Normalized Path Count}{Use \code{"npc"}.}
#'   \item{Degree-Weighted Count}{Use \code{"dwpc"}.}
#'   \item{PathSim}{Use \code{"pathsim"}.}}
#' @seealso \code{get_neighbor_list()} for neighbor reference object construction and \code{get_similarity_function()} for similarity metrics.
