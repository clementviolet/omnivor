#' Number of edge of a graph.
#'
#' @param igraph object.
#'
#' @return An integer : the number of edge of the graph.
#' @export
#'
#' @examples
#' @references Delmas, E., Besson, M., Brice, M.-H., Burkle, L. A., Dalla Riva, G. V., Fortin, M.-J., … Poisot, T. (2019). Analysing ecological networks of species interactions. Biological Reviews.
nb_edge <- function(igraph){

  stopifnot(class(igraph) == "igraph")

  return(igraph::gsize(igraph))
}
