
#' @title Fit a seasonal pattern
#'
#' @description For a *Pf*PR time series, \eqn{x}, fit
#' a spline function
#'
#' @param xds_obj an `xds` xds_obj
#'
#' @return a list with the mean peak and the values
#' @export
fit_season_bottom <- function(xds_obj){

  options=list()
  options$max_ix = 0
  options <- setup_fitting_indices(xds_obj, "bottom", options)

  bottom = get_init_X(xds_obj, "bottom")
  lims = get_limits_X(xds_obj, "bottom")

  fitit <- stats::optimize(compute_gof_X, c(0,10),
                           xds_obj=xds_obj, options=options, feature = "bottom")

  X <- fitit$minimum
  xds_obj <- update_function_X(X, xds_obj, "bottom", options)
  xds_obj <- burnin(xds_obj)
  return(xds_obj)
}

#' Seasonality: `bottom` Indices
#'
#' @inheritParams setup_fitting_indices
#'
#' @returns options
#'
#' @export
#'
setup_fitting_indices.bottom = function(xds_obj, feature, options){

  options$bottom_ix = 1
  options$bottom_ixX = options$max_ix + 1:length(options$bottom_ix)
  options$max_ix = max(options$bottom_ixX)

  return(options)
}


#' Get Initial Values for Parameters
#'
#' @inheritParams get_limits_X
#'
#' @return a vector
#' @export
get_limits_X.bottom <- function(xds_obj, feature="bottom"){
  return(c(0,365))
}

#' Get Initial Values for Parameters
#'
#' @inheritParams get_init_X
#'
#' @return a vector
#' @export
get_init_X.bottom <- function(xds_obj, feature, options=list()){
  return(list(bottom = get_season(xds_obj)$bottom))
}

#' feature a function
#'
#' @inheritParams update_function_X
#'
#' @returns sum of squared differences
#' @export
update_function_X.bottom = function(X, xds_obj, feature, options){

  bottom <- get_season_bottom(xds_obj)
  bottom   <- with(options, modify_vector_X(bottom, bottom_ix, X, bottom_ixX))
  xds_obj <- change_season_bottom(bottom, xds_obj, s=1)

  return(xds_obj)
}

