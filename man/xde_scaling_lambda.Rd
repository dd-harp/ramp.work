% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scaling.R
\name{xde_scaling_lambda}
\alias{xde_scaling_lambda}
\title{Compute lambda from an eirpr object using the Ross-Macdonald model}
\usage{
xde_scaling_lambda(model)
}
\arguments{
\item{model}{a list that defines an `ramp.xds` model (*e.g.*,  generated by `xds_setup()`)}
}
\description{
This function computes `m` and `lambda` for the output of one
of `xde_scaling_eir` or `xde_scaling_Z`.  The outputs are attached to eirpr
}
