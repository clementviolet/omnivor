#' Shortest trophic level
#'
#' Shortest trophic level is calculated as follow : 1 + the shortest chain length from a consumer to a basal species.
#' @param graph an igraph object.
#'
#' @return dataframe contening the taxons and the trophic level.
#' @export
#'
#' @examples
#' @references Williams, R.J. & Martinez, N.D., 2004. Limits to Trophic Levels and Omnivory in Complex Food Webs: Theory and Data. The American Naturalist
short_tl <- function(graph){

  stopifnot(class(graph) == "igraph")

  deg_graph <- igraph::degree(graph, mode = "out")

  consumers <- names(deg_graph[deg_graph > 0])

  ressources <- names(deg_graph[deg_graph == 0])

  tl_df <- as.data.frame(matrix(nrow = length(consumers) + length(ressources),
                                ncol = 1 + length(ressources)))

  names(tl_df) <- c("Predator", paste0("Ressource_", ressources))

  for(cons in 1:length(consumers)){

    tl_df[cons, 1] <- consumers[cons]

    temp <- suppressWarnings(igraph::shortest_paths(graph, from = igraph::V(graph)[consumers[cons]], to = igraph::V(graph)[ressources], output = "vpath"))

    temp[sapply(temp, is.null)] <- NULL

    for(res in 1:length(ressources)){ # Add path length from each predator to each basal source

      tl_df[cons, res+1] <- length(temp$vpath[[res]])

    }
  }

  for(res in 1:length(ressources)){ # Add path of one for basal species

    tl_df[cons + res, 2] <- 1
    tl_df[cons + res, 1] <- ressources[res]
  }

  tl_df[tl_df == 0] <- NA # 0 == impossible path for shortest_path, so transform it to NA

  tl_list <- split(tl_df, f = tl_df$Predator) # Each row is now splitted into a df with 1 one row and all columns

  tl_list <- lapply(tl_list, function (x) x[,!is.na(x)]) # Remove all columns with NA

  tl_list <- lapply(tl_list, function(x) x[,-1])

  tl_list <- lapply(tl_list, function(x) x[which.min(x)]) # Get the minimum path

  tl_df <- data.frame(taxon = names(tl_list),
                      trophic_level = unname(sapply(tl_list, function(x) unlist(x))),
                      stringsAsFactors = FALSE)

  tl_df <- tl_df[dim(tl_df)[1]:1,] # Reverse order of rows to match average_tl matrix row order
  row.names(tl_df) <- NULL # Arrange rowname

  return(tl_df)
}

#' Prey-averaged trophic level
#'
#' Prey-averaged trophic level is calculated as follow :
#' \deqn{TL_j = 1 + \sum_{i = 1}{S}l_{ij}\frac{TL_{i}}{n_j}}
#' With :
#' \itemize{
#'     \item \eqn{TL_j} the trophic level of the \eqn{j} specie;
#'     \item \eqn{S} the number of species of the trophque chain;
#'     \item \eqn{l_{ij}} equal 1 if the specie \eqn{j} consume specie \eqn{i};
#'     \item \eqn{n_j} the number of specie consumed by the specie \eqn{j}.
#' }
#' @param graph an igraph object.
#'
#' @return dataframe contening the taxons and the trophic level.
#' @export
#'
#' @examples
#' @references Levine, S., 1980. Several measures of trophic structure applicable to complex food webs. Journal of Theoretical Biology
#' 
#' Williams, R.J. & Martinez, N.D., 2004. Limits to Trophic Levels and Omnivory in Complex Food Webs: Theory and Data. The American Naturalist


prey_avg_tl <- function(graph){
  
  stopifnot(class(graph) == "igraph")
  
  # Should loops be removed?
  # 
  # if(any(igraph::is.loop(graph))){
  #   warning("There is some loops in the graph: loops have been remove for the calculration.", immediate. = TRUE)
  #   graph <- igraph::simplify(graph, remove.multiple = TRUE, remove.loops = TRUE)
  # }
  
  if(is.null(igraph::edge_attr(graph, name = "value"))){
    
    mat <- as.data.frame(as.matrix(igraph::as_adjacency_matrix(graph)))
    
  } # Check if there is an attribute value contenaing the value of the interaction
  else{
    
    mat <- as.data.frame(as.matrix(igraph::as_adjacency_matrix(graph, attr = "value")))
    
  }
  
  Q <- mat
  
  
  if(all(unlist(Q) == 0 | unlist(Q) == 1)){ # Check if the values of the matrix are stored in presence/absence (TRUE) or in proportion of dietary.
    
    for(i in 1:ncol(mat)){
      
      Q[i, ] <- mat[i, ] / rowSums(mat)[i] # Diet percentage of each species
      
    }
    
  } 
  
  Q[is.na(Q)] <- 0
  
  y <- c(solve(diag(ncol(Q)) - Q) %*% rep.int(1, times = ncol(Q)))
  
  tl_df <- data.frame(taxon = row.names(Q), trophic_level = y)
  
  tl_df
}

#' Short-weighted trophic level
#'
#' Short-weighted trophic level is the average of shortest trophic level and prey-averaged trophic level.
#' @param graph an igraph object.
#'
#' @return dataframe contening the taxons and the trophic level.
#' @export
#'
#' @examples
#' @references Williams, R.J. & Martinez, N.D., 2004. Limits to Trophic Levels and Omnivory in Complex Food Webs: Theory and Data. The American Naturalist
short_wght_tl <- function(graph){

  stopifnot(class(graph) == "igraph")

  short.tl <- short_tl(graph)

  prey.avg <- prey_avg_tl(graph)

  tl_df <- data.frame(taxon = prey.avg$taxon, trophic_level = rowMeans(cbind(short.tl$trophic_level, prey.avg$trophic_level)))

  return(tl_df)

}
