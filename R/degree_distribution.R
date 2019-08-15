#' Degree distribution
#' 
#' Compute the degree distribution of a network
#' @param graph an igraph object.
#' @param ... others arguments passed to \code{\link[=degree]{igraph::degree_distribution}}
#'
#' @return named numeric : a numeric vector of the degree distribution.
#' @export
#'
#' @examples
#' data(aleutian)
#' 
#' degree_distribution(aleutian)
#' @references Delmas, E., Besson, M., Brice, M.-H., Burkle, L. A., Dalla Riva, G. V., Fortin, M.-J., … Poisot, T. (2019). Analysing ecological networks of species interactions. Biological Reviews.
degree_distribution <- function(graph, ...){
  
  stopifnot(class(graph) == "igraph")
  
  res <- igraph::degree_distribution(graph, ...)
  
  names(res) <- seq.int(0, length(res)-1, 1)
  
  res
}
