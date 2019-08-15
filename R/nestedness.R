#' Nestedness
#'
#' Nestedness of a graph.
#' 
#' \code{nestedness()} return the nestedness of a network rows with regard to the columns or vice-versa.
#'
#' \code{nestedness_total()} return the whole nestedness as the mean of the two values.
#' 
#' @param graph an \code{igraph} object.
#' @param transpose \code{logical}. If \code{FALSE} the nestedness of rows with regard to the columns is comptute. If \code{TRUE} the nestedness of columns with regard to rows is compute.
#'
#' @return \code{numeric} the nestedness.
#' @export
#' @examples
#' data(aleutian)
#' 
#' nestedness(aleutian)
nestedness <- function(graph, transpose = FALSE){
  
  stopifnot(class(graph) == "igraph")
  
  graph       <- igraph::as_adjacency_matrix(graph, sparse = FALSE)
  
  if(transpose == TRUE){
    graph <- t(graph)
  }
  
  numerator   <- numeric(length = ncol(graph) - 1)
  denominator <- numeric(length = ncol(graph) - 1)
  
  for(i in 1:ncol(graph)-1){
    
    numerator[i]   <- sum(graph[i, ] != 0 & graph[i+1, ] != 0)
    
    denominator[i] <- min(sum(graph[i, ]), sum(graph[i+1, ]))
    
  }
  
  sum(numerator) / sum(denominator)
  
}

#' @rdname nestedness
#' @examples
#' nestedness_total(aleutian)
#' @export
nestedness_total <- function(graph){

  stopifnot(class(graph) == "igraph")
  
  mean(nestedness(graph), nestedness(graph))
}