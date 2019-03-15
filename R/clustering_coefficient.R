#' Cluestering coefficient
#'
#' Probability that two taxa linked to the same taxon are linked.
#' @param graph an igraph object.
#' @param type caracter: the type of the transitivity to calculate. See \code{?igraph::transitivity} for more informations.
#' @param ... others arguments passed to \code{igraph::transitivity}.
#'
#' @return numeric: the clustering coefficient.
#' @export
#'
#' @examples
#' @references Baiser, B., Gotelli, N. J., Buckley, H. L., Miller, T. E., & Ellison, A. M. (2012). Geographic variation in network structure of a nearctic aquatic food web. Global Ecology and Biogeography.
cl <- function(graph, type = "global", ...){

  stopifnot(class(graph) == "igraph" | is.character(type))

  return(igraph::transitivity(graph, type = type, eval(substitute(alist(...)))))
}
