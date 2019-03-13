#' Linkage density
#'
#' @param igraph
#'
#' @return A numeric, the linkage denisty of the graph.
#' @export
#'
#' @examples
#' @references Delmas, E., Besson, M., Brice, M.-H., Burkle, L. A., Dalla Riva, G. V., Fortin, M.-J., … Poisot, T. (2019). Analysing ecological networks of species interactions. Biological Reviews.
link_density <- function(igraph){

  stopifnot(class(igraph) == "igraph")

  return(igraph::gsize(igraph)/igraph::gorder(igraph))
}
