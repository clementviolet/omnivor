#' Number of nodes of in a graph
#'
#' Number of nodes of a graph.
#' @param graph an igraph object.
#'
#' @return integer: the number of node of the graph.
#' @export
#'
#' @examples
#' data(aleutian)
#' 
#' nb_node(aleutian)
#' @references Delmas, E., Besson, M., Brice, M.-H., Burkle, L. A., Dalla Riva, G. V., Fortin, M.-J., … Poisot, T. (2019). Analysing ecological networks of species interactions. Biological Reviews.
nb_node <- function(graph){

  stopifnot(class(graph) == "igraph")

  return(igraph::gorder(graph))
}
