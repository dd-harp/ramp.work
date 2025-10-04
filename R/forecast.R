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
