#' Characteristic path length
#'
#' The mean shortest set of links between species species pairs.
#' @param graph an igraph object.
#' @param directed logical: is the graph directed?
#' @param ... others arguments passed to \code{igraph::mean_distance}.
#'
#' @return numeric: the characteristic path length.
#' @export
#'
#' @examples
#' @references Baiser, B., Gotelli, N. J., Buckley, H. L., Miller, T. E., & Ellison, A. M. (2012). Geographic variation in network structure of a nearctic aquatic food web. Global Ecology and Biogeography.
path_length <- function(graph, directed = TRUE, ...){

  stopifnot(class(graph) == "igraph" | is.logical(directed))

  return(igraph::mean_distance(graph, directed = directed, eval(substitute(alist(...)))))
}
