#' Connectance of a graph
#'
#' Proportion of all possible links that are realized.
#' @param graph an igraph object.
#' @param loops logical: are species able to interact with themselves?
#' @param directed logical: are interactions directed?
#' @param unipartite not implemented yet
#'
#' @return numeric: the value of the connectance.
#' @export
#'
#' @examples
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
