#' Linkage density
#'
#' Number of link per species.
#' @param graph an igraph object.
#'
#' @return numeric: the linkage denisty of the graph.
#' @export
#'
#' @examples
#' data(aleutian)
#' 
#' link_density(aleutian)
#' @references Delmas, E., Besson, M., Brice, M.-H., Burkle, L. A., Dalla Riva, G. V., Fortin, M.-J., … Poisot, T. (2019). Analysing ecological networks of species interactions. Biological Reviews.
link_density <- function(graph){

  stopifnot(class(graph) == "igraph")

  return(igraph::gsize(graph)/igraph::gorder(graph))
}
