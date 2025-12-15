
#' @title Reconstruct a history of exposure from a PR time series
#'
#' @description Construct a function describing the EIR, including
#' the mean EIR, the seasonal pattern, and the i
#'
#' For set of paired a time series \eqn{X,} compute the
#' phase of a seasonal pattern for the EIR
#'
#' @param xds_obj an `xds` xds_obj
#' @param twice cycle through a second time
#'
#' @return the **`ramp.xds`** model object, fitted to a time series.
#' The state at the end is saved as `xds_obj$history`
#' @export
pr2history_xm = function(xds_obj, twice=FALSE){

  #print("trend-1")
  xds_obj <- fit_trend(xds_obj)
  #print("season-1")
  xds_obj <- fit_season(xds_obj)
  xds_obj <- norm_trend(xds_obj)

  if(twice == TRUE){
    xds_obj <- fit_trend(xds_obj)
    xds_obj <- fit_season(xds_obj)
    xds_obj <- norm_trend(xds_obj)
  }

  #print("trend-2")
  xds_obj <- fit_trend(xds_obj)

  #print("save")
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
pr2history = function(xds_obj, fit_method=NULL){

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
save_pr2history = function(xds_obj){
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
restore_pr2history = function(xds_obj){
  xds_obj <- change_mean_forcing(xds_obj$history$mean_forcing, xds_obj)
  xds_obj <- change_season(xds_obj$history$season, xds_obj)
  xds_obj$data_obj     <- xds_obj$history$data_obj
  xds_obj$fit_obj  <- xds_obj$history$fit_obj
  xds_obj$hindcast <- xds_obj$history$hindcast
  xds_obj$forecast <- xds_obj$history$forecast
  return(xds_obj)
}
