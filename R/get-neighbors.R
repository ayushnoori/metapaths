#' Get node neighbors of specified type.
#'
#' Retrieve the neighbors of a given node which are of a given type.
#'
#' @param root ID of the root node.
#' @param type Desired type of the neighbors.
#' @param eslist Edge list as a \code{data.table} which must contain the columns \code{subject},
#' \code{object}, \code{subject_type}, and \code{object_type}.
#' @return List of neighbors.
#' @author Ayush Noori
#' @export
get_neighbors = function(root, type, eslist) {

  neighbors_object = eslist[subject == root & object_type == type]
  neighbors_subject = eslist[object == root & subject_type == type]
  neighbors = c(neighbors_object[, object], neighbors_subject[, subject])
  if(identical(neighbors, character(0))) return(NA) else return(neighbors)

}
