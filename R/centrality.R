#' Degreee centrality
#' 
#' Degree centrality is the count of the number of interaction for each node. It can be partitioned between in-degree and out-degree for directed networks.
#'
#' @param graph an \code{igraph} object.
#' @param ... others arguments passed to \code{\link[=degree]{igraph::degree()}}
#'
#' @return \code{numeric} a vector containing the degree of each node.
#' @export
#'
#' @examples
#' @references Delmas, E., Besson, M., Brice, M.-H., Burkle, L. A., Dalla Riva, G. V., Fortin, M.-J., … Poisot, T. (2019). Analysing ecological networks of species interactions. Biological Reviews.
degree_centrality <- function(graph, ...){
  
  igraph::degree(graph, ...)
}

#' Closeness centrality
#'
#' Closeness centrality is a measure of the proximity of a node to all others.
#'
#' @param graph an \code{igraph} object.
#' @param ... others arguments passed to \code{\link[=closeness]{igraph::closeness()}}
#'
#' @return \code{numeric} a vector containing closeness centrality coefficients for each node.
#' @export
#'
#' @examples
#' @references Delmas, E., Besson, M., Brice, M.-H., Burkle, L. A., Dalla Riva, G. V., Fortin, M.-J., … Poisot, T. (2019). Analysing ecological networks of species interactions. Biological Reviews.
closeness_centrality <- function(graph, ...){
  
  igraph::closeness(graph, mode = "all", ...)
  
}



#' Betweeness centrality
#'
#'#' Betweeness centrality is a measure of the number of time a node is between pairs of nodes. The value can be normalized by the number, see details for more informations.
#'
#' @param graph an \code{igraph} object.
#' @param normalized \code{logical} should the coefficients be normalized? The formula would be: \deqn{C_{Bnorm} = 2 \times \frac{C_{B}}{(n-1)(n-2)}} Where \eqn{C_{B}} is the betweeness coefficent vector and \eqn{n} is the number of nodes.
#' @param ... others arguments passed to \code{\link[=betweenness]{igraph::betweenness()}}
#'
#' @return \code{numeric} a vector containing betweeness centrality coefficients for each node.
#' @export
#'
#' @examples
#' @references Delmas, E., Besson, M., Brice, M.-H., Burkle, L. A., Dalla Riva, G. V., Fortin, M.-J., … Poisot, T. (2019). Analysing ecological networks of species interactions. Biological Reviews.
betweeness_centrality <- function(graph, normalized = TRUE, ...){
  
  igraph::betweenness(graph, normalized = normalized, ...)

}


#' Eigenvector centrality
#'
#' Eigenvector centrality is a meaure of the relative importance of each node in the network by giving a score based on the nomber of links of each node.
#'
#' @param graph an \code{igraph} object.
#' @param full \code{logical} should all the \code{\link[=eigen_centrality]{igraph::eigen_centrality()}} output be returned or only the scores?
#' @param ... others arguments passed to \code{\link[=eigen_centrality]{igraph::eigen_centrality()}}
#'
#' @return \code{numeric} a vector containing eigenvector centrality coefficients for each node.
#' \code{list} see \code{\link[=eigen_centrality]{igraph::eigen_centrality()}} documentation for more informations.
#' @export
#'
#' @examples
#' @references Delmas, E., Besson, M., Brice, M.-H., Burkle, L. A., Dalla Riva, G. V., Fortin, M.-J., … Poisot, T. (2019). Analysing ecological networks of species interactions. Biological Reviews.
eigenvector_centrality <- function(graph, full = FALSE, ...){
  if(full == FALSE){
    
    igraph::eigen_centrality(graph)$vector
    
  }else{
    
    igraph::eigen_centrality(graph, ...)
    
  }
  
}
