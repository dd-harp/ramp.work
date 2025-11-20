
#' @title Fit a seasonal pattern
#'
#' @description For a *Pf*PR time series, \eqn{x}, fit
#' a spline function
#'
#' @param xds_obj an `xds` xds_obj
#'
#' @return a list with the mean peak and the values
#' @export
fit_season_pw <- function(xds_obj){

  options=list()
  options$max_ix = 0
  options <- setup_fitting_indices(xds_obj, "pw", options)

  pw = get_init_X(xds_obj, "pw")
  lims = get_limits_X(xds_obj, "pw")

  fitit <- stats::optimize(compute_gof_X, lims,
                           xds_obj=xds_obj, options=options, feature = "pw")

  X <- fitit$minimum
  xds_obj <- update_function_X(X, xds_obj, "pw", options)
  xds_obj <- burnin(xds_obj)
  return(xds_obj)
}

#' Seasonality: `pw` Indices
#'
#' @inheritParams setup_fitting_indices
#'
#' @returns options
#'
#' @export
#'
setup_fitting_indices.pw= function(xds_obj, feature, options){

  options$pw_ix = 1
  options$pw_ixX = options$max_ix + 1
  options$max_ix = max(options$pw_ixX)

  return(options)
}


#' Get Initial Values for Parameters
#'
#' @inheritParams get_limits_X
#'
#' @return a vector
#' @export
get_limits_X.pw <- function(xds_obj, feature="pw"){
  return(c(0.2,10))
}

#' Get Initial Values for Parameters
#'
#' @inheritParams get_init_X
#'
#' @return a vector
#' @export
get_init_X.pw <- function(xds_obj, feature, options=list()){
  return(list(pw=get_season(xds_obj)$pw))
}

#' feature a function
#'
#' @inheritParams update_function_X
#'
#' @returns sum of squared differences
#' @export
update_function_X.pw = function(X, xds_obj, feature, options){

  pw <- sigX(X[options$pw_ixX], 0.2, 10)
  xds_obj <- change_season(list(pw=pw), xds_obj, 1)

  return(xds_obj)
}
