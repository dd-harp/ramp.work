
#' @title Fit interannual variability using splines
#'
#' @description For a *Pf*PR time series, \eqn{x}, fit
#' a spline function. To fit only a subset of the interpolation
#' points, define `options$trend_ix`
#'
#' @param xds_obj an `xds` xds_obj
#' @param options a list to configure the indices
#'
#' @return a list with the mean peak and the values
#' @export
fit_trend <- function(xds_obj, options = list()){
  options$max_ix = 0
  options <- setup_fitting_indices(xds_obj, "trend", options)

  if(length(options$trend_ix) == 1){
    lims <- get_limits_X(xds_obj, "trend")
    fitit <- stats::optimize(compute_gof_X, interval = lims,
                             feature = "trend",
                             options=options, xds_obj=xds_obj)
    X <- fitit$minimum
  } else {
    inits = get_init_X(xds_obj, "trend", options)
    fitit <- stats::optim(inits, compute_gof_X,
                          feature="trend",
                          options=options, xds_obj=xds_obj)
    X <- fitit$par
  }

  xds_obj <- update_function_X(X, xds_obj, "trend", options)

  xds_obj <- burnin(xds_obj)
  return(xds_obj)
}

#' Setup indices for
#'
#' @inheritParams setup_fitting_indices
#'
#' @returns indices
#' @export
setup_fitting_indices.trend = function(xds_obj, feature, options){
  if(is.null(options$trend_ix))
    options$trend_ix=c(1:length(xds_obj$data_obj$yy))

  options$trend_ixX = options$max_ix + 1:length(options$trend_ix)
  options$max_ix = max(options$trend_ixX)
  return(options)
}

#' Get Initial Values for Parameters
#'
#' @inheritParams get_init_X
#'
#' @return a vector
#' @export
get_init_X.trend <- function(xds_obj, feature, options){
   return(xds_obj$data_obj$yy[options$trend_ix])
}

#' Get Initial Values for Parameters
#'
#' @inheritParams get_limits_X
#'
#' @return a vector
#' @export
get_limits_X.trend <- function(xds_obj, feature){
  return(c(.001,10))
}

#' Update the Trend Function
#'
#' @inheritParams update_function_X
#'
#' @returns sum of squared differences
#' @export
update_function_X.trend = function(X, xds_obj, feature, options){
  with(options,{
    xds_obj$data_obj$yy[trend_ix] = X[trend_ixX]
    xds_obj <- update_fit_trend(xds_obj)
    return(xds_obj)
})}

