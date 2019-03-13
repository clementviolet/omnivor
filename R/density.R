#' Linkage density
#'
#' @param graph
#'
#' @return A numeric, the linkage denisty of the graph.
#' @export
#'
#' @examples
#' @references Delmas, E., Besson, M., Brice, M.-H., Burkle, L. A., Dalla Riva, G. V., Fortin, M.-J., … Poisot, T. (2019). Analysing ecological networks of species interactions. Biological Reviews.
link_density <- function(graph){

  stopifnot(class(graph) == "igraph")

  return(igraph::gsize(graph)/igraph::gorder(graph))
}
