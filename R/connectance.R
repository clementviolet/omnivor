#' Connectance of a graph
#'
#' Proportion of all possible links that are realized. In a directed unipartite network \eqn{C = \frac{L}{S^2}}, in a directed newtork where species cannot interact with themselves \eqn{C = \frac{L}{S(S-1)}}. In undirected newtork, if species cannot interact with themselves \eqn{C = \frac{L}{S\frac{S-1}{2}}} otherwise \eqn{C = \frac{L}{S\frac{S-1}{2}}}. Where \eqn{L} is the number of links and \eqn{S} the number of species.
#' @param graph an igraph object.
#' @param loops logical: are species able to interact with themselves?
#' @param directed logical: are interactions directed?
#'
#' @return numeric: the value of the connectance.
#' @export
#'
#' @examples
#' data(aleutian)
#' 
#' connectance(aleutian)
#' 
#' connectance(aleutian, loops = FALSE)
#' 
#' connectance(aleutian, directed = FALSE)
#' @references Delmas, E., Besson, M., Brice, M.-H., Burkle, L. A., Dalla Riva, G. V., Fortin, M.-J., … Poisot, T. (2019). Analysing ecological networks of species interactions. Biological Reviews.
connectance <- function(graph, loops = TRUE, directed = TRUE){

  stopifnot(class(graph) == "igraph" | is.logical(loops) | is.logical(directed))

  if(loops & directed){

    res <- igraph::gsize(graph)/(igraph::gorder(graph)^2)

  } else if(!loops & directed){

    res <- igraph::gsize(graph)/(igraph::gorder(graph)*(igraph::gorder(graph)-1))

  } else if(loops & !directed){

    res <- igraph::gsize(graph)/(igraph::gorder(graph)*((igraph::gorder(graph)+1)/2))

  } else {

    res <- igraph::gsize(graph)/(igraph::gorder(graph)*((igraph::gorder(graph)-1)/2))

  }

  return(res)

}
