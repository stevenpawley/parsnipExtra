% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fnn.R
\name{fnn_train}
\alias{fnn_train}
\title{Nearest neighbors using FNN}
\usage{
fnn_train(x, y = NULL, k = 1, algorithm = "kd_tree", ...)
}
\arguments{
\item{x}{a data frame or matrix of predictors.}

\item{y}{a vector (factor or numeric) or matrix (numeric) of outcome data.}

\item{k}{a vector (integer) of the number of neighbours to consider.}

\item{algorithm}{character, one of c("kd_tree", "cover_tree", "brute"),
default = "kd_tree"}

\item{...}{additional arguments to pass to FNN, currently unused.}
}
\value{
list containing the FNN call
}
\description{
`fnn_train` is a wrapper for `FNN` fast nearest neighbor models
}
