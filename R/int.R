#' Percentage of intermediate taxa
#'
#' Percentage of taxa with both consumer and ressources.
#' @param graph an \code{igraph} object.
#'
#' @return \code{numeric} percentage of intermediate taxa.
#' @export
#'
#' @examples
#' data(aleutian)
#' 
#' inter(aleutian)
#' @references Baiser, B., Gotelli, N. J., Buckley, H. L., Miller, T. E., & Ellison, A. M. (2012). Geographic variation in network structure of a nearctic aquatic food web. Global Ecology and Biogeography.
inter <- function(graph){

  stopifnot(class(graph) == "igraph")

  return(100 - omnivor::basal(graph) - omnivor::top(graph))
}
