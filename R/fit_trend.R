
#' @title Fit interannual variability using splines
#'
#' @description For a *Pf*PR time series, \eqn{x}, fit
#' a spline function
#'
#' @param xds_obj an `xds` xds_obj
#' @param ix index of the \eqn{y} values of interpolation points
#'
#' @return a list with the mean peak and the values
#' @export
fit_trend <- function(xds_obj, ix=c()){

  if(length(ix)==0) ix=c(1:length(xds_obj$data$yy))
  inits = get_init_X(xds_obj, "trend")[ix]

  if(length(ix) == 1){
    lims <- get_limits_X(xds_obj, "trend")
    fitit <- stats::optimize(compute_gof_X, interval = lims,
                             update = "trend",
                             ix=ix, xds_obj=xds_obj)
    X <- fitit$minimum
  } else {
    fitit <- stats::optim(inits, compute_gof_X,
                          update="trend",
                          ix=ix, xds_obj=xds_obj)
    X <- fitit$par
  }

  xds_obj <- update_function_X(X, xds_obj, "trend", ix)
  xds_obj <- burnin(xds_obj)
  return(xds_obj)
}

#' Get Initial Values for Parameters
#'
#' @inheritParams get_init_X
#'
#' @return a vector
#' @export
get_init_X.trend <- function(xds_obj, update="trend", ix=c()){
  yy <- xds_obj$data$yy
  if(length(ix)>0) yy <- yy[ix]
  return(yy)
}

#' Get Initial Values for Parameters
#'
#' @inheritParams get_limits_X
#'
#' @return a vector
#' @export
get_limits_X.trend <- function(xds_obj, update="trend"){
  return(c(.001,10))
}

#' Update a function
#'
#' @inheritParams update_function_X
#'
#' @returns sum of squared differences
#' @export
update_function_X.trend = function(X, xds_obj, update="trend", ix = c()){
  if(length(ix)==0) ix=c(1:length(xds_obj$data$yy))
  spline_y <- xds_obj$data$yy
  spline_y <- modify_vector_X(X, ix, spline_y)
  xds_obj$data$yy  <- spline_y
  xds_obj <- update_fitting_ty(xds_obj)
  xds_obj <- set_spline(xds_obj$fitting, xds_obj)
  xds_obj  <- update_F_trend(xds_obj)
  return(xds_obj)
}


#' @title Update Interpolation Points
#'
#' @description
#' This function copies the interpolation points from the
#' `hindcast` and `data` and `forecast` to `fitting.`
#'
#' During fitting, the algorithm changes
#' one or more of the \eqn{y} values of the
#' interpolation points in
#' `xds_obj$data$yy.` The xds_obj then calls
#' `hindcast_ty` and `forecast_ty` to update `xds_obj$hindcast$y` (the
#' pre-observation interpolation points for burn-in) and `xds_obj$hindcast$y`
#' (post-observation interpolation points).
#' These are copied to the
#' full set of control points stored at
#' at `xds_obj$fitting$tt` and `xds_obj$fitting$yy.`
#'
#' The rules for `hindcast_ty`  and `forecast_ty`
#' are setup
#' by `setup_hindcast` and `setup_forecast.`
#'
#'
#' @param xds_obj a **`ramp.xds`** xds_obj object
#'
#' @returns a **`ramp.xds`** xds_obj object
#' @export
update_fitting_ty = function(xds_obj){
  xds_obj <- hindcast_ty(xds_obj)
  xds_obj <- forecast_ty(xds_obj)
  with(xds_obj,{
    xds_obj$fitting$tt = c(hindcast$tt, data$tt, forecast$tt)
    xds_obj$fitting$yy = c(hindcast$yy, data$yy, forecast$yy)
  return(xds_obj)
  })}
