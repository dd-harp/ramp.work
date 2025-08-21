
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

  xds_obj <- fit_model(xds_obj, "season")
  xds_obj <- fit_model(xds_obj, "trend")

  if(twice == TRUE){
    xds_obj <- fit_model(xds_obj, "season")
    xds_obj <- fit_model(xds_obj, "trend")
  }

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
#'
#' @return the **`ramp.xds`** model object, fitted to a time series.
#' The state at the end is saved as `xds_obj$history`
#' @export
pr2history = function(xds_obj){

  xds_obj <- fit_model(xds_obj, c("season", "trend"))
  xds_obj <- save_pr2history(xds_obj)

  return(xds_obj)
}

#' @title Save the history fitting
#'
#' @description After fitting a model
#' to data, save the history. To restore
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
  xds_obj$history$data     <- xds_obj$data
  xds_obj$history$fitting  <- xds_obj$fitting
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
  xds_obj <- set_mean_forcing(xds_obj$history$mean_forcing, xds_obj)
  xds_obj <- set_season(xds_obj$history$season, xds_obj)
  xds_obj$data     <- xds_obj$history$data
  xds_obj$fitting  <- xds_obj$history$fitting
  xds_obj$hindcast <- xds_obj$history$hindcast
  xds_obj$forecast <- xds_obj$history$forecast
  return(xds_obj)
}
