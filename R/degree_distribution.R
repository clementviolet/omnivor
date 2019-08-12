#' Degree distribution
#' 
#' Compute the degree distribution of a network
#' @param graph an igraph object.
#' @param ... others arguments passed to \code{igraph::degree_distribution}
#'
#' @return named numeric : a numeric vector of the degree distribution.
#' @export
#'
#' @examples
#' @references Delmas, E., Besson, M., Brice, M.-H., Burkle, L. A., Dalla Riva, G. V., Fortin, M.-J., … Poisot, T. (2019). Analysing ecological networks of species interactions. Biological Reviews.
degree_distribution <- function(graph, ...){
  
  stopifnot(class(graph) == "igraph")
  
  res <- igraph::degree_distribution(graph, ...)
  
  names(res) <- seq.int(0, length(res)-1, 1)
  
  res
}
