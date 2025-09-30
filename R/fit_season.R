#' @title Fit a seasonal pattern
#'
#' @description For a *Pf*PR time series, \eqn{x}, fit
#' a spline function
#'
#' @param xds_obj an `xds` xds_obj
#'
#' @return a list with the mean peak and the values
#' @export
fit_season <- function(xds_obj){

  options = list()
  options$max_ix = 0
  options = setup_fitting_indices(xds_obj, "season", options)

  inits = get_init_X(xds_obj, "season")

  fitit <- stats::optim(inits, compute_gof_X,
                        xds_obj=xds_obj, options=options,
                        feature="season")

  X <- fitit$par
  browser()
  xds_obj <- update_function_X(X, xds_obj, "season", options)
  xds_obj <- burnin(xds_obj)
  return(xds_obj)
}

#' Seasonality Indices
#'
#' @inheritParams setup_fitting_indices
#'
#' @returns options
#'
#' @export
#'
setup_fitting_indices.season = function(xds_obj, feature, options){

  options = setup_fitting_indices(xds_obj, "pw", options)
  options = setup_fitting_indices(xds_obj, "bottom", options)
  options = setup_fitting_indices(xds_obj, "phase", options)

  return(options)
}

#' Get Initial Values for Parameters
#'
#' @inheritParams get_init_X
#'
#' @return a vector
#' @export
get_init_X.season <- function(xds_obj, feature, options=list()){
  return(with(get_season(xds_obj), list(pw=pw, bottom=bottom, phase=phase)))
}

#' @title feature `F_season`
#'
#' @description This features three shape parameters
#' for a function `F_season` using [modify_vector_X] and [update_F_season]
#'
#' @note This assumes the vector, `X`, has got
#' three sets of parameters describing three
#' parameters in a seasonal function with
#' parameter names phase, bottom, and pw.
#'
#' @inheritParams update_function_X
#'
#' @returns sum of squared differences
#' @export
update_function_X.season = function(X, xds_obj, feature="season", options=list()){

  phase <- get_season_phase(xds_obj)
  phase <- with(options, modify_vector_X(phase, phase_ix, X, phase_ixX))

  bottom <- get_season_bottom(xds_obj)
  bottom <- with(options, modify_vector_X(bottom, bottom_ix, X, bottom_ixX))

  pw <- get_season_pw(xds_obj)
  pw <- with(options, modify_vector_X(pw, pw_ix, X, pw_ixX))

  xds_obj <- change_season(list(bottom=bottom, pw=pw, phase=phase), xds_obj, s=1)

  return(xds_obj)
}



