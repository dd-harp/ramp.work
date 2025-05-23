% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/effect_size.R
\name{sse_effect_sizes}
\alias{sse_effect_sizes}
\title{Given data, compute GoF for a spline function}
\usage{
sse_effect_sizes(X, data, times, model, irs_rounds, itn_rounds)
}
\arguments{
\item{data}{the PR observed}

\item{times}{the times of the observations}

\item{model}{the model}

\item{irs_rounds}{a list to configure irs_multiround}

\item{itn_rounds}{a list to configure itn_multiround}
}
\value{
a list with the mean peak and the values
}
\description{
For a time series c(`times`,`data`),
compute the sum of squared errors for a seasonal
pattern defined by `Fpar`
}
