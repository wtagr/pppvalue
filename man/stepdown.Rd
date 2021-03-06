% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stepdown.R
\name{stepdown}
\alias{stepdown}
\title{Single-step/Step-down adjusted p-values}
\usage{
stepdown(realvec, permat)
}
\arguments{
\item{realvec}{A vector of observed data.}

\item{permat}{A matrix of simulated/permuted data, the \code{nrow} of which must be the same as the length of 
\code{realvec} and the \code{ncol} of which denotes the number of the simulation or permutation.}
}
\value{
%% ~Describe the value returned %% If it is a LIST, use %%
\item{p1}{Estimated single-step adjusted p-values}
\item{p2 }{Estimated step-down adjusted p-values}
}
\description{
Estimate the single-step/step-down adjusted p-values (internal function).
}
\seealso{
\code{\link{pppvalue}}
}
\author{
Meng Xu, Philip Reiss
}
\keyword{internal}
