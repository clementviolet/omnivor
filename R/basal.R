#' Percentage of basal species
#'
#' Percentage of species without ressources.
#' @param graph an igraph object.
#'
#' @return numeric: percentage of basal species.
#' @export
#'
#' @examples
#' @references
basal <- function(graph){

  stopifnot(class(graph) == "igraph")

  res <- igraph::degree(graph, mode = "out") # out == node only receiving interactions

  return(length(res[res == 0])/igraph::gorder(graph)*100)
}
