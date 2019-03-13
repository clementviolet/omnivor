#' Number of nodes of a graph.
#'
#' @param graph object.
#'
#' @return An integer: the number of node of the graph.
#' @export
#'
#' @examples
#' @references Delmas, E., Besson, M., Brice, M.-H., Burkle, L. A., Dalla Riva, G. V., Fortin, M.-J., … Poisot, T. (2019). Analysing ecological networks of species interactions. Biological Reviews.
nb_node <- function(graph){

  stopifnot(class(graph) == "igraph")

  return(igraph::gorder(graph))
}
