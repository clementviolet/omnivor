% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/basal.R
\name{basal}
\alias{basal}
\title{Percentage of basal species}
\usage{
basal(graph)
}
\arguments{
\item{graph}{an igraph object.}
}
\value{
numeric: percentage of basal species.
}
\description{
Percentage of species without ressources.
}
\examples{
data(aleutian)

basal(aleutian)
}
\references{
Baiser, B., Gotelli, N. J., Buckley, H. L., Miller, T. E., & Ellison, A. M. (2012). Geographic variation in network structure of a nearctic aquatic food web. Global Ecology and Biogeography.
}
