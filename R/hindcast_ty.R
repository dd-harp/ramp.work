
#' @title Hindcast a Baseline
#'
#' @description This function adds N interpolation
#' points to the spline parameters *before* an
#' observation period for burn-in. The \eqn{t}
#' values are set at yearly intervals, and the
#' \eqn{y} values that serve as a basis. See
#' \href{https://dd-harp.github.io/ramp.work/vignettes/Hindcast.html}{Hindcasting}
#'
#'
#' @param N the number of years to forecast
#' @param model a **`ramp.xds`** model object
#' @param method to dispatch set_pre_obs_ty
#' @param impute_ty to dispatch `impute_baseline_ty`
#' @param trust_ty to dispatch `get_trust_ty`
#'
#' @returns a **`ramp.xds`** model object
#'
#' @export
#'
setup_hindcast = function(model, N=0,
                    method = "value",
                    impute_ty = "mean",
                    trust_ty = "first"){
  hindcast <- list()

  class(method) <- method
  hindcast$method <- method

  hindcast$impute_ty <- impute_ty
  hindcast$trust_ty <- trust_ty

  if(N==0) N=length(model$fitting$pre$tt)
  hindcast$N <- N


  pre_yrs = min(model$data$years) - c(N:1)

  hindcast$yrs = pre_yrs
  hindcast$tt = pre_yrs*365
  hindcast$yy = rep(1, N)
  model$hindcast <- hindcast

  model <- hindcast_ty(model)

  return(model)
}

#' @title Hindcast a Baseline
#'
#' @description This function adds N interpolation
#' points to the spline parameters *before* an
#' observation period for burn-in. The \eqn{t}
#' values are set at yearly intervals, and the
#' \eqn{y} values that serve as a basis
#'
#'
#' @param model a **`ramp.xds`** model object
#' @param ix (optional) indices for [impute_value]
#'
#' @returns a **`ramp.xds`** model object
#'
#' @export
#'
hindcast_ty = function(model, ix=c()){with(model$hindcast,{
  yy <- impute_value(ix, model, impute_ty, trust_ty, N)
  model  <- set_pre_obs_y(yy, model)
  return(model)
})}

#' Set up a hindcast for burn-in
#'
#' @param y the y values for interpolation
#' @param model a **`ramp.xds** model object
#'
#' @returns a **`ramp.xds** model object
#' @export
set_pre_obs_y = function(y, model){
  UseMethod("set_pre_obs_y", model$hindcast$method)
}


#' Set up the pre-observation interpolating points
#'
#' @note Set the \eqn{y} value for
#' pre-observation interpolation points
#' to a constant.
#'
#' @inheritParams set_pre_obs_y
#'
#' @returns a **`ramp.xds** model object
#' @export
set_pre_obs_y.value = function(y, model){
  N <- model$hindcast$N
  model$hindcast$yy = rep(y, N)
  return(model)
}

#' Set up the pre-observation interpolating points
#'
#' @note Use "all" as the include_rule
#' @inheritParams set_pre_obs_y
#'
#' @returns a **`ramp.xds** model object
#' @export
set_pre_obs_y.mirror = function(y, model){
#  Not working yet
#  N <- model$fitting$hindcast$N
#  model$fitting$pre$tt = -rev(ty$tt-min(ty$tt))-365
#  model$fitting$pre$yy = rev(ty$yy)
  return(model)
}
