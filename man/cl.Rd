% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clustering_coefficient.R
\name{cl}
\alias{cl}
\title{Cluestering coefficient}
\usage{
cl(graph, type = "global", ...)
}
\arguments{
\item{graph}{an igraph object.}

\item{type}{caracter: the type of the transitivity to calculate. See \code{?igraph::transitivity} for more informations.}

\item{...}{others arguments passed to \code{igraph::transitivity}.}
}
\value{
numeric: the clustering coefficient.
}
\description{
Probability that two taxa linked to the same taxon are linked.
}
\examples{
data(aleutian)

cl(aleutian)
}
\references{
Baiser, B., Gotelli, N. J., Buckley, H. L., Miller, T. E., & Ellison, A. M. (2012). Geographic variation in network structure of a nearctic aquatic food web. Global Ecology and Biogeography.
}
