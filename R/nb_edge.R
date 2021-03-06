#' Number of edge of in a graph
#'
#' Number of edge of in a graph.
#' @param graph an igraph object.
#'
#' @return integer: the number of edge of the graph.
#' @export
#'
#' @examples
#' data(aleutian)
#' 
#' nb_edge(aleutian)
#' @references Delmas, E., Besson, M., Brice, M.-H., Burkle, L. A., Dalla Riva, G. V., Fortin, M.-J., … Poisot, T. (2019). Analysing ecological networks of species interactions. Biological Reviews.
nb_edge <- function(graph){

  stopifnot(class(graph) == "igraph")

  return(igraph::gsize(graph))
}
