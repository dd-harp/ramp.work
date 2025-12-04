
#' @title Reconstruct a history of exposure from a PR time series
#'
#' @description Construct a function describing the EIR, including
#' the mean EIR, the seasonal pattern, and the i
#'
#' For set of paired a time series \eqn{X,} compute the
#' phase of a seasonal pattern for the EIR
#'
#' @param xds_obj an `xds` xds_obj
#' @param bednet_ix the indices for bednet events to fit
#' @param irs_ix the indices for irs events to fit
#' @param N cycle through a N times
#'
#' @return the **`ramp.xds`** model object, fitted to a time series.
#' The state at the end is saved as `xds_obj$history`
#' @export
pr2shockit_xm = function(xds_obj, bednet_ix = c(), irs_ix=c(), N=1){

  N=N-1
  print("trend-1")
  xds_obj <- fit_trend(xds_obj)
  print("season-1")
  xds_obj <- fit_season(xds_obj)
  print("bednet-1")
  if(length(bednet_ix)>0) xds_obj <- fit_bednet_shock(xds_obj, list(bednet_ix=bednet_ix))
  print("irs-1")
  if(length(irs_ix>0)) xds_obj <- fit_irs_shock(xds_obj, list(irs_ix=irs_ix))

  if(N>0){
    N=N-1
    xds_obj <- fit_trend(xds_obj)
    xds_obj <- fit_season(xds_obj)
    if(length(irs_ix>0)) xds_obj <- fit_irs_shock(xds_obj,list(bednet_ix=bednet_ix))
    if(length(bednet_ix)>0) xds_obj <- fit_bednet_shock(xds_obj, list(irs_ix=irs_ix))
  }

  print("trend-2")
  xds_obj <- fit_trend(xds_obj)

  print("save")
  xds_obj <- save_pr2shockit(xds_obj)

  return(xds_obj)
}


#' @title Reconstruct a history of exposure from a PR time series
#'
#' @description Construct a function describing the EIR, including
#' the mean EIR, the seasonal pattern, and the i
#'
#' For set of paired a time series \eqn{X,} compute the
#' phase of a seasonal pattern for the EIR
#'
#' @param xds_obj an `xds` xds_obj
#' @param fit_method the method for [optim]
#'
#' @return the **`ramp.xds`** model object, fitted to a time series.
#' The state at the end is saved as `xds_obj$history`
#' @export
pr2shockit = function(xds_obj, fit_method=NULL){

  xds_obj <- fit_model(xds_obj, c("season", "trend"), fit_method=fit_method)
  xds_obj <- save_pr2shockit(xds_obj)

  return(xds_obj)
}

#' @title Save the history fit_obj
#'
#' @description After fit_obj a model
#' to data_obj, save the history. To restore
#' that history, use []
#'
#'
#' For set of paired a time series \eqn{X,} compute the
#' phase of a seasonal pattern for the EIR
#' @note This utility relies on `xds_cohort`
#'
#' @param xds_obj an `xds` xds_obj
#'
#' @return an `xds` object
#' @export
save_pr2shockit = function(xds_obj){
  xds_obj$history = list()
  xds_obj$mean_forcing <- get_mean_forcing(xds_obj)
  xds_obj$season <- get_season(xds_obj)
  xds_obj$history$data_obj <- xds_obj$data_obj
  xds_obj$history$fit_obj  <- xds_obj$fit_obj
  xds_obj$history$hindcast <- xds_obj$hindcast
  xds_obj$history$forecast <- xds_obj$forecast
  return(xds_obj)
}

#' @title Reconstruct a history of exposure from a PR time series
#'
#' @description Construct a function describing the EIR, including
#' the mean EIR, the seasonal pattern, and the i
#'
#' For set of paired a time series \eqn{X,} compute the
#' phase of a seasonal pattern for the EIR
#'
#' @note This utility relies on `xds_scaling`
#'
#' @param xds_obj an `xds` xds_obj
#'
#' @return an `xds` object
#' @export
restore_pr2shockit = function(xds_obj){
  xds_obj <- change_mean_forcing(xds_obj$history$mean_forcing, xds_obj)
  xds_obj <- change_season(xds_obj$history$season, xds_obj)
  xds_obj$data_obj     <- xds_obj$history$data_obj
  xds_obj$fit_obj  <- xds_obj$history$fit_obj
  xds_obj$hindcast <- xds_obj$history$hindcast
  xds_obj$forecast <- xds_obj$history$forecast
  return(xds_obj)
}
