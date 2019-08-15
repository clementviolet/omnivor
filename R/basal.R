#' Percentage of basal species
#'
#' Percentage of species without ressources.
#' @param graph an igraph object.
#'
#' @return numeric: percentage of basal species.
#' @export
#'
#' @examples
#' data(aleutian)
#' 
#' basal(aleutian)
#' @references Baiser, B., Gotelli, N. J., Buckley, H. L., Miller, T. E., & Ellison, A. M. (2012). Geographic variation in network structure of a nearctic aquatic food web. Global Ecology and Biogeography.
basal <- function(graph){

  stopifnot(class(graph) == "igraph")

  res <- igraph::degree(graph, mode = "out") # out == node only receiving interactions

  return(length(res[res == 0])/igraph::gorder(graph)*100)
}
