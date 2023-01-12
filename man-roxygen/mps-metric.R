#' @param mps List of meta-paths, where each meta-path is a vector of node types (e.g., \code{c("TypeA", "TypeB", "TypeC")}).
#' @param metric A vector of permissible similarity metrics (e.g., \code{c("npc", "dwpc")}), see defined metrics below:
#' \describe{
#'   \item{Path Count}{Specify \code{"pc"}, corresponding to \code{get_pc()}.}
#'   \item{Normalized Path Count}{Specify \code{"npc"}, corresponding to \code{get_npc()}.}
#'   \item{Degree-Weighted Path Count}{Specify \code{"dwpc"}, corresponding to \code{get_dwpc()}.}
#'   \item{PathSim}{Specify \code{"pathsim"}, corresponding to \code{get_pathsim()}.}}
