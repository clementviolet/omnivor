#' Number of nodes of a graph.
#'
#' @param igraph object.
#'
#' @return An integer: the number of node of the graph.
#' @export
#'
#' @examples
#' @references Delmas, E., Besson, M., Brice, M.-H., Burkle, L. A., Dalla Riva, G. V., Fortin, M.-J., … Poisot, T. (2019). Analysing ecological networks of species interactions. Biological Reviews.
nb_node <- function(igraph){

  stopifnot(class(igraph) == "igraph")

  return(igraph::gorder(igraph))
}
