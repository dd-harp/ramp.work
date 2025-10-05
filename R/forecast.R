#' @title Set the forecast interpolation
#'
#' @param xds_obj a **`ramp.xds`** xds_obj object
#' @param N the number of years to forecast
#' @param method to dispatch setup_post_obs_y
#'
#' @returns a **`ramp.xds`** xds_obj object
#'
#' @export
setup_forecast = function(xds_obj, N=0,
                          method = "use_last"){

  forecast <- list()

  class(method) <- method
  forecast$method = method

  if(N==0) N=length(xds_obj$forecast$tt)
  forecast$N <- N


  post_yrs = max(xds_obj$data$years) + c(1:N)
  forecast$yrs = post_yrs
  forecast$tt = post_yrs*365
  forecast$yy = rep(1, N)

  xds_obj$forecast <- forecast

  xds_obj <- forecast_ty(xds_obj)

  return(xds_obj)
}

#' @title Set the forecast interpolation
#'
#' @param xds_obj a **`ramp.xds`** xds_obj object
#' @param ix (optional) indices for [impute_value]
#'
#' @returns a **`ramp.xds`** xds_obj object
#'
#' @export
forecast_ty = function(xds_obj, ix=c()){with(xds_obj$forecast,{
  yy <- xds_obj$data$yy
  xds_obj <- setup_post_obs_y(yy, xds_obj)
  return(xds_obj)
})}

#' @title Forecast a Baseline
#'
#' @param y the interpolation points
#' @param xds_obj a **`ramp.xds`** xds_obj object
#'
#' @returns a **`ramp.xds`** xds_obj object
#'
#' @export
setup_post_obs_y = function(y, xds_obj){
  UseMethod("setup_post_obs_y", xds_obj$forecast$method)
}

#' @title Set forecast interpolation points
#'
#' @inheritParams setup_post_obs_y
#'
#' @returns a **`ramp.xds`** xds_obj object
#'
#' @export
setup_post_obs_y.use_last = function(y, xds_obj){
  N <- xds_obj$forecast$N
  xds_obj$forecast$yy = rep(tail(y,1), N)
  return(xds_obj)
}

#' @title Set forecast interpolation points
#'
#' @inheritParams setup_post_obs_y
#'
#' @returns a **`ramp.xds`** xds_obj object
#'
#' @export
setup_post_obs_y.asis= function(y, xds_obj){
  xds_obj$fitting$post$yy = y
  return(xds_obj)
}

#' @title Make a Forecast from Trusted Values
#'
#' @description Use trusted values from a fitted model
#' to generate a new set of interpolation points
#'
#' @param trusted_ix the index (or indices) of a node (or nodes) to replace
#' @param N the number of years to forecast
#' @param xds_obj a **`ramp.xds`** model object
#' @param impute_y a text string to dispatch `impute_value`
#'
#' @returns an **`ramp.xds`** model object
#'
#' @export
change_forecast_ix = function(xds_obj, trusted_ix, N=3, impute_y ="subsamp"){
  xds_obj$forecast$method = "asis"
  class(xds_obj$forecast$method) = "asis"
  xds_obj$data$yy[trusted_ix] -> trusted_y
  max_t = max(xds_obj$data$tt)
  xds_obj$forecast$tt = max_t + 365*c(1:N)
  max_yr = max(xds_obj$data$years)
  xds_obj$forecast$yrs = max_yr + c(1:3)
  new_yy = impute_value(trusted_y, impute_y, N)
  xds_obj$forecast$yy = new_yy
  xds_obj$forecast$N = N
  xds_obj <- update_fitting_ty(xds_obj)
  xds_obj <- setup_trend_par(xds_obj)
  return(xds_obj)
}

#' @title Impute the baseline
#'
#' @description Impute the a new \eqn{y} value(s) for one or more
#' interpolation points
#'
#' @param xds_obj a **`ramp.xds`** model object
#' @param t new \eqn{t}-values for the forecast interpolation points
#' @param y new \eqn{y}-values for the forecast interpolation points
#'
#' @returns an **`ramp.xds`** model object
#'
#' @export
change_forecast_ty = function(xds_obj, t, y){
  stopifnot(length(t)==length(y))
  xds_obj$forecast$method = "asis"
  class(xds_obj$forecast$method) = "asis"
  xds_obj$forecast$yy = y
  xds_obj$forecast$tt = t
  xds_obj$forecast$N = length(t)
  xds_obj$forecast$yrs = floor(t/365)
  xds_obj <- update_fitting_ty(xds_obj)
  xds_obj <- setup_trend_par(xds_obj)
  return(xds_obj)
}
