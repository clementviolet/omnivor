#' Number of nodes of a graph.
#'
#' @param igraph object.
#'
#' @return `integer` The number of node of the graph.
#' @export
#'
#' @examples
nb_node <- function(igraph){

  stopifnot(class(igraph) == "igraph")

  return(igraph::gorder(igraph))
}
