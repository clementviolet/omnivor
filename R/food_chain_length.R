#' Mean trophic level
#'
#' Mean trophic level for the food web. Also called "mean food chain length".
#'
#' @param graph an igraph object.
#'
#' @return numeric: the mean trophic level
#' @export
#'
#' @examples
#' @references Baiser, B., Gotelli, N. J., Buckley, H. L., Miller, T. E., & Ellison, A. M. (2012). Geographic variation in network structure of a nearctic aquatic food web. Global Ecology and Biogeography.
mean_tl <- function(graph){

  stopifnot(class(graph) == "igraph")

  res <- mean(short_wght_tl(graph)[, 2])

  return(res)
}
