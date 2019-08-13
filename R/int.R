#' Percentage of intermediate taxa
#'
#' Percentage of taxa with both consumer and ressources.
#' @param graph an \code{igraph} object.
#'
#' @return \code{numeric} percentage of intermediate taxa
#' @export
#'
#' @examples
#' @references
inter <- function(graph){

  stopifnot(class(graph) == "igraph")

  return(100 - omnivor::basal(graph) - omnivor::top(graph))
}
