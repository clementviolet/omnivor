#' Percentage of cannibals
#'
#' Percentage of cannibals species in a food web.
#' @param graph an igraph object.
#'
#' @return numeric: Percentage of cannibals
#' @export
#'
#' @examples
#' data(aleutian)
#' 
#' can(aleutian)
#' @references Baiser, B., Gotelli, N. J., Buckley, H. L., Miller, T. E., & Ellison, A. M. (2012). Geographic variation in network structure of a nearctic aquatic food web. Global Ecology and Biogeography.
can <- function(graph){

  stopifnot(class(graph) == "igraph")

  sum(igraph::is.loop(graph)) / igraph::gsize(graph) * 100
}
