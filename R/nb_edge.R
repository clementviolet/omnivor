#' Number of edge of a graph.
#'
#' @param igraph object.
#'
#' @return `integer` The number of edge of the graph.
#' @export
#'
#' @examples
nb_edge <- function(igraph){

  stopifnot(class(igraph) == "igraph")

  return(igraph::gsize(igraph))
}
