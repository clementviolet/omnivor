#' Retrive if a taxon is omnivore.
#'
#' Retrive if a taxon is omnivore or not, based on shortest trophic level method.
#'
#' @param graph an igraph object.
#'
#' @return dataframe. First column containing the taxa and second if the taxon is omnivore or not.
#' @export
#'
#' @examples
#' @references Baiser, B., Gotelli, N. J., Buckley, H. L., Miller, T. E., & Ellison, A. M. (2012). Geographic variation in network structure of a nearctic aquatic food web. Global Ecology and Biogeography.
omnivore <- function(graph, method = c("shortest", "average_prey")){

  stopifnot(class(graph) == "igraph")

  method <- match.arg(method, c("shortest", "average_prey"))

  if(method == "shortest"){

    cat("Methode: \"shortest trophic level\"")

    omni <- omnivore_short_tl(graph)

    omni <- sum(omni$Omnivore) / nrow(omni) * 100

  } else {

    cat("Methode: \"average prey trophic level\"")

    omni <- omnivore_avg_prey_tl(graph)

    omni <- sum(omni$Omnivore) / nrow(omni) * 100
  }

  return(omni)
}

#' Retrive if a taxon is omnivore
#'
#' Retrive if a taxon is omnivore or not, based on average prey trophic level or shortest trophic level method.
#'
#' @param graph an igraph object.
#'
#' @return dataframe. First column containing the taxa and second if the taxon is omnivore or not.
#' @export
#'
#' @examples
#' @references Baiser, B., Gotelli, N. J., Buckley, H. L., Miller, T. E., & Ellison, A. M. (2012). Geographic variation in network structure of a nearctic aquatic food web. Global Ecology and Biogeography.
which_omnivore <- function(graph, method = c("shortest", "average_prey")){

  stopifnot(class(graph) == "igraph")

  method <- match.arg(method, c("shortest", "average_prey"))

  if(method == "shortest"){

    cat("Methode: \"shortest trophic level\"")

    omnivore_short_tl(graph)

  } else {
    cat("Methode: \"average prey trophic level\"")

    omnivore_avg_prey_tl(graph)
  }
}

#' Retrive if a taxon is omnivore.
#'
#' Retrive if a taxon is omnivore or not, based on shortest trophic level method.
#'
#' @param graph an igraph object.
#'
#' @return dataframe. First column containing the taxa and second if the taxon is omnivore or not.
#' @export
#'
#' @examples
#' @keywords internal
omnivore_short_tl <- function(graph){

  tl <- omnivor::short_tl(graph)

  mat <- as.data.frame(as.matrix(igraph::as_adjacency_matrix(graph))) # The consumers are in lines, preys in columns.

  mat <- mat[rowSums(mat) > 0, ] # Remove basal species

  predator_name <- row.names(mat)

  res <- data.frame(Predator = predator_name,
                    Omnivore = rep(NA, times = length(predator_name)),
                    stringsAsFactors = FALSE)

  for(pred in 1:length(predator_name)){

    temp <- mat[pred, ]

    prey_name <- names(which(colSums(temp) >0))

    if(is.na(var(tl[tl$taxon %in% prey_name, 2]))){ # Case of a predator using only one ressource

      res[res$Predator == predator_name[pred], 2] <- FALSE

    } else if(var(tl[tl$taxon %in% prey_name, 2]) == 0){

      res[res$Predator == predator_name[pred], 2] <- FALSE

    } else {

      res[res$Predator == predator_name[pred], 2] <- TRUE

    }
  }

  return(res)
}

#' Retrive if a taxon is omnivore
#'
#' Retrive if a taxon is omnivore or not, based on average prey trophic level method.
#'
#' @param graph an igraph object.
#'
#' @return dataframe. First column containing the taxa and second if the taxon is omnivore or not.
#' @export
#'
#' @examples
#' @keywords internal
omnivore_avg_prey_tl <- function(graph){

  tl <- prey_avg_tl(graph)

  mat <- as.data.frame(as.matrix(igraph::as_adjacency_matrix(graph))) # The consumers are in lines, preys in columns.

  mat <- mat[rowSums(mat) > 0, ] # Remove basal species

  predator_name <- row.names(mat)

  res <- data.frame(Predator = predator_name,
                    Omnivore = rep(NA, times = length(predator_name)),
                    stringsAsFactors = FALSE)

  for(pred in 1:length(predator_name)){

    temp <- mat[pred, ]

    prey_name <- names(which(colSums(temp) >0))

    if(is.na(var(tl[tl$taxon %in% prey_name, 2]))){ # Case of a predator using only one ressource

      res[res$Predator == predator_name[pred], 2] <- FALSE

    } else if(var(tl[tl$taxon %in% prey_name, 2]) == 0){

      res[res$Predator == predator_name[pred], 2] <- FALSE

    } else {

      res[res$Predator == predator_name[pred], 2] <- TRUE

    }
  }

  return(res)
}
