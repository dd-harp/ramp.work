% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trend.R
\name{sse_splinef}
\alias{sse_splinef}
\title{Given data, compute GoF for a spline function}
\usage{
sse_splinef(Fpar, data, times, model)
}
\arguments{
\item{Fpar}{parameters defining a spline function}

\item{data}{the PR observed}

\item{times}{the times of the observations}

\item{model}{the model}
}
\value{
a list with the mean peak and the values
}
\description{
For a time series c(`times`,`data`),
compute the sum of squared errors for a seasonal
pattern defined by `Fpar`
}
