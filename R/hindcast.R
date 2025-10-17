
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
#' @param method to dispatch setup_hindcast_ty
#'
#' @returns a **`ramp.xds`** xds_obj object
#'
#' @export
#'
setup_hindcast = function(xds_obj, N=0,
                          method = "use_first"){
  hindcast <- list()

  class(method) <- method
  hindcast$method <- method

  if(N==0) N=length(xds_obj$fitting$pre$tt)
  hindcast$N <- N

  pre_yrs = min(xds_obj$data_obj$yr0) - c(N:1)

  hindcast$yrs = pre_yrs
  hindcast$tt = -c(N:1)*365
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
  yy <- xds_obj$data$yy
  xds_obj  <- setup_hindcast_y(yy, xds_obj)
  return(xds_obj)
})}

#' Set up a hindcast for burn-in
#'
#' @param y the y values for interpolation
#' @param xds_obj a **`ramp.xds** xds_obj object
#'
#' @returns a **`ramp.xds** xds_obj object
#' @export
setup_hindcast_y = function(y, xds_obj){
  UseMethod("setup_hindcast_y", xds_obj$hindcast$method)
}


#' Set up the pre-observation interpolating points
#'
#' @note Set the \eqn{y} value for
#' pre-observation interpolation points
#' to a constant.
#'
#' @inheritParams setup_hindcast_y
#'
#' @returns a **`ramp.xds** xds_obj object
#' @export
setup_hindcast_y.use_first = function(y, xds_obj){
  N <- xds_obj$hindcast$N
  xds_obj$hindcast$yy = rep(y[1], N)
  return(xds_obj)
}

#' Set up the pre-observation interpolating points
#'
#' @note Set the \eqn{y} value for
#' pre-observation interpolation points
#' to a constant.
#'
#' @inheritParams setup_hindcast_y
#'
#' @returns a **`ramp.xds** xds_obj object
#' @export
setup_hindcast_y.asis = function(y, xds_obj){
  return(xds_obj)
}

#' Set up the pre-observation interpolating points
#'
#' @note Use "all" as the include_rule
#' @inheritParams setup_hindcast_y
#'
#' @returns a **`ramp.xds** xds_obj object
#' @export
setup_hindcast_y.mirror = function(y, xds_obj){
  xds_obj$hindcast$yy = rev(y)
  xds_obj$hindcast$N = length(y)
  return(xds_obj)
}
