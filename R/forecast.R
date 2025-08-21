#' @title Set the forecast interpolation
#'
#' @param model a **`ramp.xds`** model object
#' @param N the number of years to forecast
#' @param method to dispatch set_post_obs_y
#' @param impute_ty to dispatch `impute_baseline_ty`
#' @param trust_ty to dispatch `get_trusted_ty`
#'
#' @returns a **`ramp.xds`** model object
#'
#' @export
setup_forecast = function(model, N=0,
                          method = "value",
                          impute_ty = "last",
                          trust_ty = "last"){

  forecast <- list()

  class(method) <- method
  forecast$method = method

  forecast$impute_ty <- impute_ty
  forecast$trust_ty <- trust_ty

  if(N==0) N=length(model$forecast$tt)
  forecast$N <- N


  post_yrs = max(model$data$years) + c(1:N)
  forecast$yrs = post_yrs
  forecast$tt = post_yrs*365
  forecast$yy = rep(1, N)

  model$forecast <- forecast

  model <- forecast_ty(model)

  return(model)
}

#' @title Set the forecast interpolation
#'
#' @param model a **`ramp.xds`** model object
#' @param ix (optional) indices for [impute_value]
#'
#' @returns a **`ramp.xds`** model object
#'
#' @export
forecast_ty = function(model, ix=c()){with(model$forecast,{
  yy <- impute_value(ix, model, impute_ty, trust_ty, N)
  model <- set_post_obs_y(yy, model)
  return(model)
})}

#' @title Forecast a Baseline
#'
#' @param y the interpolation points
#' @param model a **`ramp.xds`** model object
#'
#' @returns a **`ramp.xds`** model object
#'
#' @export
set_post_obs_y = function(y, model){
  UseMethod("set_post_obs_y", model$forecast$method)
}

#' @title Set forecast interpolation points
#'
#' @inheritParams set_post_obs_y
#'
#' @returns a **`ramp.xds`** model object
#'
#' @export
set_post_obs_y.value = function(y, model){
  N <- model$forecast$N
  model$forecast$yy = rep(y, N)
  return(model)
}

#' @title Set forecast interpolation points
#'
#' @inheritParams set_post_obs_y
#'
#' @returns a **`ramp.xds`** model object
#'
#' @export
set_post_obs_y.asis= function(y, model){
  model$fitting$post$yy = y
  return(model)
}
