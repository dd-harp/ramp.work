
#' @title Fit interannual variability using splines
#'
#' @description For a *Pf*PR time series, \eqn{x}, fit
#' a spline function. To fit only a subset of the interpolation
#' points, define `options$shock_ix`
#'
#' @param xds_obj an `xds` xds_obj
#' @param options a list to configure the indices
#'
#' @return a list with the mean peak and the values
#' @export
fit_shock <- function(xds_obj, options = list()){
  options$max_ix = 0
  options <- setup_fitting_indices(xds_obj, "shock", options)

  if(length(options$shock_ix) == 1){
    lims <- get_limits_X(xds_obj, "shock")
    fitit <- stats::optimize(compute_gof_X, interval = lims,
                             feature = "shock",
                             options=options, xds_obj=xds_obj)
    X <- fitit$minimum
  } else {
    inits = get_init_X(xds_obj, "shock", options)
    fitit <- stats::optim(inits, compute_gof_X,
                          feature="shock",
                          options=options, xds_obj=xds_obj)
    X <- fitit$par
  }

  xds_obj <- update_function_X(X, xds_obj, "shock", options)

  xds_obj <- burnin(xds_obj)
  return(xds_obj)
}

#' Setup indices for
#'
#' @inheritParams setup_fitting_indices
#'
#' @returns indices
#' @export
setup_fitting_indices.shock = function(xds_obj, feature, options){
  if(is.null(options$shock_ix))
    options$shock_ix=c(1:length(xds_obj$data$yy))

  options$shock_ixX = options$max_ix + 1:length(options$shock_ix)
  options$max_ix = max(options$shock_ixX)
  return(options)
}

#' Get Initial Values for Parameters
#'
#' @inheritParams get_init_X
#'
#' @return a vector
#' @export
get_init_X.shock <- function(xds_obj, feature, options){
  with(options,{
    yy <- xds_obj$data$yy[shock_ix]
    return(yy)
  })}

#' Get Initial Values for Parameters
#'
#' @inheritParams get_limits_X
#'
#' @return a vector
#' @export
get_limits_X.shock <- function(xds_obj, feature){
  return(c(.001,10))
}

#' Update the shock Function
#'
#' @inheritParams update_function_X
#'
#' @returns sum of squared differences
#' @export
update_function_X.shock = function(X, xds_obj, feature, options){
  spline_y <- xds_obj$data$yy
  spline_y <- with(options, modify_vector_X(spline_y, shock_ix, X, shock_ixX))
  xds_obj$data$yy  <- spline_y
  xds_obj <- update_fitting_ty(xds_obj)
  xds_obj <- change_spline_y(xds_obj$fitting$yy, xds_obj)
  return(xds_obj)
}
