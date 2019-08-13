#' Diameter of a network
#' 
#' Diameter of a network.
#'
#' @param graph an \code{igraph} object.
#' @param ... others arguments passed to \code{\link[=diameter]{igraph::diameter}}
#'
#' @return \code{numeric}: the diameter of the graph.
#' @export
#'
#' @examples
#' @references Delmas, E., Besson, M., Brice, M.-H., Burkle, L. A., Dalla Riva, G. V., Fortin, M.-J., … Poisot, T. (2019). Analysing ecological networks of species interactions. Biological Reviews.
diameter <- function(graph, ...){
  
  stopifnot(class(graph) == "igraph")
  
  igraph::diameter(graph, ...)
  
}
