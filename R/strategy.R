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
#' @references Baiser, B., Gotelli, N. J., Buckley, H. L., Miller, T. E., & Ellison, A. M. (2012). Geographic variation in network structure of a nearctic aquatic food web. Global Ecology and Biogeography.
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
#' @references Baiser, B., Gotelli, N. J., Buckley, H. L., Miller, T. E., & Ellison, A. M. (2012). Geographic variation in network structure of a nearctic aquatic food web. Global Ecology and Biogeography.
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
#' @references Baiser, B., Gotelli, N. J., Buckley, H. L., Miller, T. E., & Ellison, A. M. (2012). Geographic variation in network structure of a nearctic aquatic food web. Global Ecology and Biogeography.
gen_sd <- function(graph){

  stopifnot(class(graph) == "igraph")

  return(sd(gen(graph)[, 2])*link_density(graph))

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
#' @references Baiser, B., Gotelli, N. J., Buckley, H. L., Miller, T. E., & Ellison, A. M. (2012). Geographic variation in network structure of a nearctic aquatic food web. Global Ecology and Biogeography.
vul_sd <- function(graph){

  stopifnot(class(graph) == "igraph")

  return(sd(vul(graph)[, 2])*link_density(graph))
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
#' @references Baiser, B., Gotelli, N. J., Buckley, H. L., Miller, T. E., & Ellison, A. M. (2012). Geographic variation in network structure of a nearctic aquatic food web. Global Ecology and Biogeography.
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
#' @return
#' @export
#'
#' @examples
#' @references Baiser, B., Gotelli, N. J., Buckley, H. L., Miller, T. E., & Ellison, A. M. (2012). Geographic variation in network structure of a nearctic aquatic food web. Global Ecology and Biogeography.
link_sd <- function(graph){

  stopifnot(class(graph) == "igraph")

  return(sd(link(graph)[, 2]*link_density(graph)))

}
