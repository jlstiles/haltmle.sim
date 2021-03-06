% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cv_sl.R
\name{get_or_fit}
\alias{get_or_fit}
\title{Fit a learner on training folds and get predictions on validation folds}
\usage{
get_or_fit(task, folds, W, A, Y, sl_control)
}
\arguments{
\item{task}{A named list identifying what training_folds to fit the learner
on. The function returns predictions from this fit on the remaining folds (i.e.,
the validation folds).}

\item{folds}{Vector identifying which fold observations fall into.}

\item{Y}{A matrix or data.frame of outcomes.}

\item{sl_control}{A list with named entries ensemble.fn, optim_risk_fn, weight_fn,
cv_risk_fn, family. Available functions can be viewed with \code{sl_control_options()}. See
\code{?sl_control_options} for more on how users may supply their own functions.}

\item{X}{A matrix or data.frame of predictors.}
}
\value{
A named list with task and output of super learner wrapper fit.
}
\description{
Fit a learner on training folds and get predictions on validation folds
}
