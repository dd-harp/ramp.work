
#' A utility to set up indices for fitting
#'
#' @param patch_ix index across patches
#' @param trend_ix index across spline interpolation points
#' @param irs_ix index for IRS Rounds
#' @param bednet_ix index for Bed Net Distribution Rounds
#'
#' @return a list
#' @export
make_fit_ix_opts = function(patch_ix=1, trend_ix=c(), irs_ix=1, bednet_ix=1){
  list(patch_ix=patch_ix, trend_ix=trend_ix, irs_ix=irs_ix, bednet_ix=bednet_ix)
}

#' Set up indices for model fitting
#'
#' @param i an index for update
#' @param update a list of model features to update
#' @param ix_opts options for setting indices
#'
#' @returns indices
#'
#' @export
#'
make_fit_ix = function(i, update, ix_opts){
  UseMethod("make_fit_ix", update[i])
}

#' Indices for mean forcing
#'
#' @inheritParams make_fit_ix
#'
#' @returns indices
#' @export
make_fit_ix.mean_forcing= function(i, update, ix_opts){return(ix_opts$patch_ix)}

#' Indices for seasonality
#'
#' @inheritParams make_fit_ix
#'
#' @returns indices
#' @export
make_fit_ix.bottom = function(i, update, ix_opts){return(ix_opts$patch_ix)}

#' Indices for seasonality
#'
#' @inheritParams make_fit_ix
#'
#' @returns indices
#' @export
make_fit_ix.pw = function(i, update, ix_opts){return(ix_opts$patch_ix)}

#' Indices for seasonality
#'
#' @inheritParams make_fit_ix
#'
#' @returns indices
#' @export
make_fit_ix.phase = function(i, update, ix_opts){return(ix_opts$patch_ix)}

#' Indices for seasonality
#'
#' @inheritParams make_fit_ix
#'
#' @returns indices
#' @export
make_fit_ix.amplitude = function(i, update, ix_opts){return(ix_opts$patch_ix)}

#' Indices for seasonality
#'
#' @inheritParams make_fit_ix
#'
#' @returns indices
#' @export
make_fit_ix.season= function(i, update, ix_opts){return(ix_opts$patch_ix)}

#' Indices for trends
#'
#' @inheritParams make_fit_ix
#'
#' @returns indices
#' @export
make_fit_ix.trend = function(i, update, ix_opts){return(ix_opts$trend_ix)}

#' Indices for IRS Models
#'
#' @inheritParams make_fit_ix
#'
#' @returns indices
#' @export
make_fit_ix.irs_coverage = function(i, update, ix_opts){return(ix_opts$irs_ix)}

#' Indices for Bednet Models
#'
#' @inheritParams make_fit_ix
#'
#' @returns indices
#' @export
make_fit_ix.bednet_coverage = function(i, update, ix_opts){return(ix_opts$irs_ix)}
