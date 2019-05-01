#' Generality
#'
#' The number of prey of a taxon.
#'
#' @param graph an igraph object.
#'
#' @return dataframe: generality of each taxon.
#' @export
#'
#' @examples
#' @references Williams, R. J., & Martinez, N. D. (2000). Simple rules yield complex food webs. Nature.
gen <- function(graph){

  stopifnot(class(graph) == "igraph")

  node_name <- igraph::V(graph)$name

  res <- data.frame(taxon = node_name,
                    generality = rep(NA, times = length(node_name)))

  for(node in 1:length(node_name)){

    res$generality[node] <- length(igraph::E(graph)[from(node_name[node])])
  }

  return(res)
}

#' Normalized standard deviation of generality
#'
#' The number of prey of a taxon standardized by L/S.
#'
#' @param graph an igraph object.
#'
#' @return numeric: the normalized standard deviation of generality.
#' @export
#'
#' @examples
#' @references Williams, R. J., & Martinez, N. D. (2000). Simple rules yield complex food webs. Nature.
gen_sd <- function(graph){
  
  stopifnot(class(graph) == "igraph")
  
  res <- gen(graph)
  
  res[, 2] <- res[, 2] * 1 / link_density(graph)
  return(res)
  
}

#' Mean Generality
#'
#' Mean number of prey per taxon for the whole food web.
#'
#' @param graph an igraph object.
#' @param std should the number of prey per taxon be standardized by 1/(LS)? Value must be `TRUE` or `FALSE`.
#'
#' @return
#' @export
#'
#' @examples
#' @references Williams, R. J., & Martinez, N. D. (2000). Simple rules yield complex food webs. Nature.
gen_mean <- function(graph, std = FALSE){
  
  stopifnot(class(graph) == "igraph")
  
  if(std == TRUE){
    
    res <- gen_sd(graph)
    
  }else{
    res <- gen(graph)
  }
  
  mean(res[, 2])
}

#' Vulnerability
#'
#' The number of predators of a taxon.
#'
#' @param graph an igraph object.
#'
#' @return dataframe: vulnerability of each taxon.
#' @export
#'
#' @examples
#' @references Williams, R. J., & Martinez, N. D. (2000). Simple rules yield complex food webs. Nature.
vul <- function(graph){

  stopifnot(class(graph) == "igraph")

  node_name <- igraph::V(graph)$name

  res <- data.frame(taxon = node_name,
                    vulnerability = rep(NA, times = length(node_name)))

  for(node in 1:length(node_name)){

    res$vulnerability[node] <- length(igraph::E(graph)[to(node_name[node])])
  }

  return(res)
}

#' Normalized standard deviation of vulnerability
#'
#' The number of predators of a taxon standardized by L/S.
#'
#' @param graph an igraph object.
#'
#' @return numeric: the normalized standard deviation of vulnerability.
#' @export
#'
#' @examples
#' @references Williams, R. J., & Martinez, N. D. (2000). Simple rules yield complex food webs. Nature.
vul_sd <- function(graph){

  stopifnot(class(graph) == "igraph")

  res <- vul(graph)
  
  res[, 2] <- res[, 2] * (1 / link_density(graph))
  
  return(res)
}

#' Mean number of vulnerability
#'
#' Mean number of predators of a taxon for the whole food web.
#'
#' @param graph an igraph object.
#' @param std should the number of predator per taxon be standardized by 1/(LS)? Value must be `TRUE` or `FALSE`.
#'
#' @return
#' @export
#'
#' @examples
#' @references Williams, R. J., & Martinez, N. D. (2000). Simple rules yield complex food webs. Nature.
vul_mean <- function(graph, std = FALSE){
  
  stopifnot(class(graph) == "igraph")
  
  if(std == TRUE){
    
    res <- vul(graph)
    
  }else{
    res <- vul_sd(graph)
  }
  
  return(mean(res[, 2]))
}

#' Link
#'
#' Number of consumers and resources per taxon.
#'
#' @param graph an igraph object.
#'
#' @return dataframe: number of links of each taxon.
#' @export
#'
#' @examples
#' @references Williams, R. J., & Martinez, N. D. (2000). Simple rules yield complex food webs. Nature.
link <- function(graph){

  stopifnot(class(graph) == "igraph")

  node_name <- igraph::V(graph)$name

  res <- data.frame(taxon = node_name,
                    nb_link = rep(NA, times = length(node_name)))

  for(node in 1:length(node_name)){

    res$nb_link[node] <- length(igraph::E(graph)[adj(node_name[node])])
  }

  return(res)

}

#' Normalized standard deviation of links
#'
#' Number of consumers and resources per taxon standardized by L/S.
#'
#' @param graph an igraph object.
#'
#' @return dataframe: standardized number of links of each taxon.
#' @export
#'
#' @examples
#' @references Williams, R. J., & Martinez, N. D. (2000). Simple rules yield complex food webs. Nature.
link_sd <- function(graph){

  stopifnot(class(graph) == "igraph")
  
  res <- link(graph)
  
  res[, 2] <- res[, 2] * (1 / link_density(graph))
  
  return(res)

}

#' Mean number of links
#'
#' Mean number of consumers and resources per taxon for the whole food web.
#'
#' @param graph an igraph object.
#' @param std should the number of links per taxon be standardized by 1/(LS)? Value must be `TRUE` or `FALSE`.
#'
#' @return the mean number of consumers and resources per taxon for the whole food web.
#' @export
#'
#' @examples
#' @references Williams, R. J., & Martinez, N. D. (2000). Simple rules yield complex food webs. Nature.
link_mean <- function(graph, std = FALSE){
  
  stopifnot(class(graph) == "igraph")
  
  if(std == TRUE){
    
    res <- link_sd(graph)
    
  }else{
    res <- link(graph)
  }
  
  return(mean(res[, 2]))
}