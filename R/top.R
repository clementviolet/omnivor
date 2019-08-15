#' Percentage of top species
#'
#' Percentage of species without consumers.
#' @param graph an igraph object.
#'
#' @return numeric: percentage of top species.
#' @export
#'
#' @examples
#' data(aleutian)
#' 
#' top(aleutian)
#' @references Baiser, B., Gotelli, N. J., Buckley, H. L., Miller, T. E., & Ellison, A. M. (2012). Geographic variation in network structure of a nearctic aquatic food web. Global Ecology and Biogeography.
top <- function(graph){

  stopifnot(class(graph) == "igraph")

  res <- igraph::degree(graph, mode = "in") # in == node only giving interactions

  return(length(res[res == 0])/igraph::gorder(graph)*100)
}
