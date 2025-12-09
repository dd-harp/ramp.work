
#' @title Reconstruct a history of exposure from a PR time series
#'
#' @description Construct a function describing the EIR, including
#' the mean EIR, the seasonal pattern, and the i
#'
#' For set of paired a time series \eqn{X,} compute the
#' phase of a seasonal pattern for the EIR
#'
#' @param xds_obj an `xds` xds_obj
#' @param rep number of times to repeat the fitting cycle
#' @param trend_fix the indices for trend events to fix
#' @param trend_x the fixed values
#' @param bednet_fix the indices for bednet events to fix
#' @param bednet_x the fixed values
#' @param irs_fix the indices for irs events to fix
#' @param irs_x the fixed values
#'
#' @return the **`ramp.xds`** model object, fitted to a time series.
#' The state at the end is saved as `xds_obj$history`
#' @export
pr2shockfit_xm = function(xds_obj, rep=1,
                         trend_fix = c(), trend_x = 1,
                         bednet_fix = c(), bednet_x = 0,
                         irs_fix=c(), irs_x = 0){

  trend_ix = 1:length(xds_obj$data_obj$tt)
  if(length(trend_fix)>0){
    trend_ix=trend_ix[-trend_fix]
    xds_obj$data_obj$yy[trend_fix] = sqrt(trend_x)
  }

  N = xds_obj$events_obj$bednet$N
  if(N>0){
    bednet_ix = 1:N
    if(length(bednet_fix)>0){
      bednet_ix= bednet_ix[-bednet_fix]
      xds_obj$events_obj$bednet$shock[bednet_fix] = checkIt(bednet_x, length(bednet_fix))
    }
  }

  N = xds_obj$events_obj$irs$N
  if(N>0){
    irs_ix = 1:N
    if(length(irs_fix)>0){
      irs_ix = irs_ix[-irs_fix]
      xds_obj$events_obj$irs$shock[irs_fix] = checkIt(irs_x, length(irs_fix))
    }
  }

  print(paste("bednet: rep ", rep))
  print(bednet_ix)
  if(length(bednet_ix)>0)
    xds_obj <- fit_bednet_shock(xds_obj, list(bednet_ix=bednet_ix))

  print(paste("irs: rep ", rep))
  print(irs_ix)
  if(length(irs_ix)>0)
    xds_obj <- fit_irs_shock(xds_obj, list(irs_ix=irs_ix))



  while(rep>0){

    print(paste("trend: rep ", rep))
    print(c(trend_ix))
    xds_obj <- fit_trend(xds_obj, options = list(trend_ix=trend_ix))
    print(paste("season: rep ", rep))

    xds_obj <- fit_season(xds_obj)

    print(paste("irs: rep ", rep))
    print(irs_ix)
    if(length(irs_ix>0))
      xds_obj <- fit_irs_shock(xds_obj,list(irs_ix=irs_ix))

    print(paste("bednet: rep ", rep))
    print(bednet_ix)
    if(length(bednet_ix)>0)
      xds_obj <- fit_bednet_shock(xds_obj, list(bednet_ix=bednet_ix))
    rep=rep-1
  }

  print("trend-2")
  xds_obj <- fit_trend(xds_obj, options=list(trend_ix=trend_ix))

  print("save")
  xds_obj <- save_pr2history(xds_obj)

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
pr2shockfit = function(xds_obj, fit_method=NULL){

  xds_obj <- fit_model(xds_obj, c("season", "trend"), fit_method=fit_method)
  xds_obj <- save_pr2history(xds_obj)

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
