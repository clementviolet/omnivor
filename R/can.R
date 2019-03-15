#' Percentage of cannibals
#'
#' Percentage of cannibals species in a food web.
#' @param graph an igraph object.
#'
#' @return numeric: Percentage of cannibals
#' @export
#'
#' @examples
#' @references
can <- function(graph){

  stopifnot(class(graph) == "igraph")

  sum(igraph::is.loop(graph)) / igraph::gsize(graph) * 100
}
