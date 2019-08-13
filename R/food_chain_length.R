#' Mean trophic level
#'
#' Mean trophic level for the food web. Also called "mean food chain length".
#'
#' @param graph an \code{igraph} object.
#' @param method computation method for the trophic level. Must be one of `shortest`, `average_prey` `weighted`. See \code{\link{short_tl}},\code{\link{prey_avg_tl}}, \code{\link{short_wght_tl}} for more informations.
#'
#' @return numeric: the mean trophic level
#' @export
#'
#' @examples
#' @references Baiser, B., Gotelli, N. J., Buckley, H. L., Miller, T. E., & Ellison, A. M. (2012). Geographic variation in network structure of a nearctic aquatic food web. Global Ecology and Biogeography.
mean_tl <- function(graph, method = c("shortest", "average_prey", "weighted")){

  stopifnot(class(graph) == "igraph")
  
  method <- match.arg(method, c("shortest", "average_prey", "weighted"))
  
  if(method == "shortest") res <- mean(short_tl(graph)[, 2])
  if(method == "average_prey") res <- mean(prey_avg_tl(graph)[, 2])
  if(method == "weighted") res <- mean(short_wght_tl(graph)[, 2])

  return(res)
}
