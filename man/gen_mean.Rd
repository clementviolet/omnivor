% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/strategy.R
\name{gen_mean}
\alias{gen_mean}
\title{Mean Generality}
\usage{
gen_mean(graph, std = FALSE)
}
\arguments{
\item{graph}{an igraph object.}

\item{std}{should the number of prey per taxon be standardized by 1/(LS)? Value must be `TRUE` or `FALSE`.}
}
\value{
\code{numeric} mean number of prey per taxon.
}
\description{
Mean number of prey per taxon for the whole food web.
}
\examples{
data(aleutian)

gen_mean(aleutian)
}
\references{
Williams, R. J., & Martinez, N. D. (2000). Simple rules yield complex food webs. Nature.
}
