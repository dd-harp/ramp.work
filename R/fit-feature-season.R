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

  options$phase_ixX = options$max_ix + 1
  options$bottom_ixX = options$max_ix + 2
  options$pw_ixX = options$max_ix + 3

  options$max_ix = options$max_ix + 3

  return(options)
}

#' Get Initial Values for Parameters
#'
#' @inheritParams get_init_X
#'
#' @return a vector
#' @export
get_init_X.season <- function(xds_obj, feature, options=list()){
  return(with(get_season(xds_obj), c(phase=phase,
                                     bottom=sigXinv(bottom, 0, 20),
                                     pw=sigXinv(pw, .2, 10))))
}

#' @title feature `F_season`
#'
#' @description This features three shape parameters
#' for a function `F_season` using [modify_vector_X] and [change_season]
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
  with(options,{
    pars <- get_season(xds_obj)
    pars$phase <- X[phase_ixX]
    pars$bottom <- sigX(X[bottom_ixX], 0, 20)
    pars$pw <- sigX(X[pw_ixX], 0.2, 10)
    xds_obj <- change_season(pars, xds_obj, s=1)
    return(xds_obj)
})}


