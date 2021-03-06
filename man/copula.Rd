% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/copula.R
\name{copula}
\alias{copula}
\alias{copula1}
\alias{copula2}
\alias{copula3}
\title{Generate the empirical copula from a joint random sample}
\usage{
copula(joint_sample)
}
\arguments{
\item{joint_sample}{an object of type \code{2DSample}}
}
\value{
\preformatted{    The two list members are also named x and y and the list
    itself is of class type \code{2DSample}.
}

The empirical copula model of the joint sampleas a list
with two vectors of x and y of the samples coordinates.
}
\description{
\code{copula} generates the empirical copula
corresponding to the joint sample passed in
as input
}
\examples{
copula(frown(1000))

}
