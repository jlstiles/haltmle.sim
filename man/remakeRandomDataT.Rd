% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remakeRandomDataT.R
\name{remakeRandomDataT}
\alias{remakeRandomDataT}
\title{remakeRandomData}
\usage{
remakeRandomDataT(n, object, setA = NULL, setW = NULL, ...)
}
\arguments{
\item{n}{A \code{numeric} sample size}

\item{object}{An object of class \code{"makeRandomData"}}

\item{setA}{Value(s) to set treatment variable to. If \code{NULL} then treatment
is simulated according to observed data functions. If \code{length(A)==1} then all
values are set to this single value. Otherwise, if A is a vector of length n, A is set
according to this vector.}

\item{setW}{A matrix of proper size with values of covariates set to fixed levels. Useful for
plotting methods. Assumes proper dimensions. If \code{NULL} simulates W according to distributions.}
}
\value{
An object of class \code{"makeRandomData"} with the following entries
\item{W}{A matrix of covariates}
\item{A}{A vector of binary treatments}
\item{Y}{A vector of continuously valued outcome}
\item{distW}{A list containing relevant information needed to reproduce data sets}
\item{fnG0}{A list of lists containing relevant information needed to reproduce data sets}
\item{fnQ0}{A list of lists containing relevant information needed to reproduce data sets}
\item{distErrY}{A list containing relevant information needed to reproduce data sets}
}
\description{
Given an object output by \code{makeRandomData}, make another data set with the same
distribution.
}
