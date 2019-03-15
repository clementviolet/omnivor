#' Percentage of top species
#'
#' Percentage of species without consumers.
#' @param graph an igraph object.
#'
#' @return numeric: percentage of top species.
#' @export
#'
#' @examples
#' @references
top <- function(graph){

  stopifnot(class(graph) == "igraph")

  res <- igraph::degree(graph, mode = "in") # in == node only giving interactions

  return(length(res[res == 0])/igraph::gorder(graph)*100)
}
