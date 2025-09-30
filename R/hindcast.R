
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
#' @param N the number of years to hindcast
#' @param xds_obj a **`ramp.xds`** xds_obj object
#' @param method to dispatch setup_pre_obs_ty
#' @param impute_ty to dispatch `impute_baseline_ty`
#' @param trust_ty to dispatch `get_trust_ty`
#'
#' @returns a **`ramp.xds`** xds_obj object
#'
#' @export
#'
setup_hindcast = function(xds_obj,
                          N=0,
                          method = "value",
                          impute_ty = "mean",
                          trust_ty = "first"){
  hindcast <- list()

  class(method) <- method
  hindcast$method <- method

  hindcast$impute_ty <- impute_ty
  hindcast$trust_ty <- trust_ty

  if(N==0) N=length(xds_obj$fitting$pre$tt)
  hindcast$N <- N

  pre_yrs = min(xds_obj$data$years) - c(N:1)

  hindcast$yrs = pre_yrs
  hindcast$tt = pre_yrs*365
  hindcast$yy = rep(1, N)
  xds_obj$hindcast <- hindcast
  xds_obj$hindcast$t_init = min(hindcast$tt)

  xds_obj <- hindcast_ty(xds_obj)

  return(xds_obj)
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
#' @param xds_obj a **`ramp.xds`** xds_obj object
#' @param ix (optional) indices for [impute_value]
#'
#' @returns a **`ramp.xds`** xds_obj object
#'
#' @export
#'
hindcast_ty = function(xds_obj, ix=c()){with(xds_obj$hindcast,{
  yy <- impute_value(ix, xds_obj, impute_ty, trust_ty, N)
  xds_obj  <- setup_pre_obs_y(yy, xds_obj)
  return(xds_obj)
})}

#' Set up a hindcast for burn-in
#'
#' @param y the y values for interpolation
#' @param xds_obj a **`ramp.xds** xds_obj object
#'
#' @returns a **`ramp.xds** xds_obj object
#' @export
setup_pre_obs_y = function(y, xds_obj){
  UseMethod("setup_pre_obs_y", xds_obj$hindcast$method)
}


#' Set up the pre-observation interpolating points
#'
#' @note Set the \eqn{y} value for
#' pre-observation interpolation points
#' to a constant.
#'
#' @inheritParams setup_pre_obs_y
#'
#' @returns a **`ramp.xds** xds_obj object
#' @export
setup_pre_obs_y.value = function(y, xds_obj){
  N <- xds_obj$hindcast$N
  xds_obj$hindcast$yy = rep(y, N)
  return(xds_obj)
}

#' Set up the pre-observation interpolating points
#'
#' @note Set the \eqn{y} value for
#' pre-observation interpolation points
#' to a constant.
#'
#' @inheritParams setup_pre_obs_y
#'
#' @returns a **`ramp.xds** xds_obj object
#' @export
setup_pre_obs_y.asis = function(y, xds_obj){
  return(xds_obj)
}

#' Set up the pre-observation interpolating points
#'
#' @note Use "all" as the include_rule
#' @inheritParams setup_pre_obs_y
#'
#' @returns a **`ramp.xds** xds_obj object
#' @export
setup_pre_obs_y.mirror = function(y, xds_obj){
#  Not working yet
#  N <- xds_obj$fitting$hindcast$N
#  xds_obj$fitting$pre$tt = -rev(ty$tt-min(ty$tt))-365
#  xds_obj$fitting$pre$yy = rev(ty$yy)
  return(xds_obj)
}
