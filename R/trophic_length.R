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

    temp <- suppressWarnings(igraph::shortest_paths(graph, from = V(graph)[consumers[cons]], to = V(graph)[ressources], output = "vpath"))

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
#' @references Williams, R.J. & Martinez, N.D., 2004. Limits to Trophic Levels and Omnivory in Complex Food Webs: Theory and Data. The American Naturalist
prey_avg_tl <- function(graph){

  stopifnot(class(graph) == "igraph")

  mat <- as.data.frame(as.matrix(igraph::as_adjacency_matrix(graph))) # The consumers are in lines, preys in columns.

  tl_df <- data.frame(taxon = row.names(mat), # Returned df
                      trophic_length = rep(NA, nrow(mat)),
                      stringsAsFactors = FALSE)

  while(!sum(is.na(tl_df[, 2])) == 0){ # While df is not full of trophic level

    if(sum(is.na(tl_df[, 2])) == nrow(tl_df)){ # initialization of the trophic_level df for basal

      for(ressource in 1:nrow(mat[rowSums(mat) == 0,])){

        name <- row.names(mat[rowSums(mat) == 0,])

        tl <- 1 + sum(mat[name, ] * 0 / nrow(mat))

        tl_df[tl_df$taxon == name, 2] <- tl
      }
    }

    # Reshaping df to keep only row that consume ressources which trophic level was already determined.
    temp <- mat[rowSums(mat) <= length(name) & rowSums(mat[, name]) > 0 & !rownames(mat) %in% name, ]
    temp <- temp[, colSums(temp) > 0]

    for(taxon in 1:nrow(temp)){ # Calculating trophic length for

      r_name <- row.names(temp)[taxon]

      c_name <- temp[r_name, ]
      c_name <- names(c_name[which(c_name > 0)])

      tl <- 1 + sum(temp[r_name, c_name] * tl_df[tl_df$taxon %in% c_name, 2] / length(c_name))

      tl_df[tl_df$taxon == r_name, 2] <- tl
    }

    name <- tl_df[!is.na(tl_df$trophic_length),1] # Keep track of trophic level already resolved.

  }

  return(tl_df)
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

  tl_df <- data.frame(taxon = prey.avg$taxon, trophic_level = rowMeans(cbind(short.tl$trophic_level, prey.avg$trophic_length)))

  return(tl_df)

}
