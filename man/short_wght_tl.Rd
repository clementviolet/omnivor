% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trophic_length.R
\name{short_wght_tl}
\alias{short_wght_tl}
\title{Short-weighted trophic level}
\usage{
short_wght_tl(graph)
}
\arguments{
\item{graph}{an igraph object.}
}
\value{
dataframe contening the taxons and the trophic level.
}
\description{
Short-weighted trophic level is the average of shortest trophic level and prey-averaged trophic level.
}
\examples{
data(aleutian)

short_wght_tl(aleutian)
}
\references{
Williams, R.J. & Martinez, N.D., 2004. Limits to Trophic Levels and Omnivory in Complex Food Webs: Theory and Data. The American Naturalist
}
