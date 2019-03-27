#' Percentage of herbivore/detrivore species in the newtork
#'
#' @param graph an \code{igraph} object.
#'
#' @return \code{integer} the percentage of herbivore/detrivore species.
#' @export 
#'
#' @examples
#' @references Baiser, B., Gotelli, N. J., Buckley, H. L., Miller, T. E., & Ellison, A. M. (2012). Geographic variation in network structure of a nearctic aquatic food web. Global Ecology and Biogeography.
herbivory <- function(graph){

  stopifnot(class(graph) == "igraph")
  
  ressources <- igraph::degree(graph, mode = "out") # out == node only receving interactions
  ressources <- names(ressources[ressources == 0]) # Get all basal taxa.
  
  herbi <- igraph::adjacent_vertices(graph, ressources, mode ="in") # Retrive all adjacent vertices consuming ressources
  herbi <- unique(unlist(lapply(herbi, function(x) x$name)))
  
  return(length(herbi) / igraph::gorder(graph) * 100)
}