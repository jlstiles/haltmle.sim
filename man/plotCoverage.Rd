% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarizeBias.R
\name{plotCoverage}
\alias{plotCoverage}
\title{plotCoverage}
\usage{
plotCoverage(result, cilNames, ciuNames, strat, truthName = "truth",
  pointOptions = list(pch = 1:length(cilNames)), estLabels = cilNames,
  legendOptions = list(x = "topleft", legend = estLabels), ylim = NULL, ...)
}
\arguments{
\item{result}{The result data frame}

\item{cilNames}{Column name(s) of results data frame that are lower confidence bounds}

\item{ciuNames}{Column name(s) of results data frame that are upper confidence bounds}

\item{strat}{The name of the variable on which to stratify}

\item{truthName}{The column name containing the true ATE}

\item{pointOptions}{A list of arguments to be passed to points}

\item{estLabels}{Vector of names to label estimators in legend (default is to use estNames)}

\item{legendOptions}{A list of arguments passed to legend. pointOptions are added
automatically}

\item{ylim}{y-axis limits for the plot. If null, looks for range and adds 5% to it}

\item{...}{Additional arguments passed to plot}
}
\description{
Function to plot the coverage for a given set of estimators
stratified by a given set of covariates
}
