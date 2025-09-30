
#' @title Fit a seasonal pattern
#'
#' @description For a *Pf*PR time series, \eqn{x}, fit
#' a spline function
#'
#' @param xds_obj an `xds` xds_obj
#'
#' @return a list with the mean peak and the values
#' @export
fit_season_amplitude <- function(xds_obj){

  options=list()
  options$max_ix = 0
  options <- setup_fitting_indices(xds_obj, "amplitude", options)

  inits = get_init_X(xds_obj, "amplitude")

  fitit <- stats::optim(inits, compute_gof_X,
                        xds_obj=xds_obj, options=options,
                        feature="amplitude")

  X <- fitit$par
  xds_obj <- update_function_X(X, xds_obj, "amplitude", options)
  xds_obj <- burnin(xds_obj)
  return(xds_obj)
}

#' Seasonality: `amplitude` Indices
#'
#' @inheritParams setup_fitting_indices
#'
#' @returns options
#'
#' @export
#'
setup_fitting_indices.amplitude = function(xds_obj, feature, options){

  options = setup_fitting_indices(xds_obj, "pw", options)
  options = setup_fitting_indices(xds_obj, "bottom", options)

  return(options)
}

#' Get Initial Values for Parameters
#'
#' @inheritParams get_init_X
#'
#' @return a vector
#' @export
get_init_X.amplitude <- function(xds_obj, feature, options=list()){
  season_par <- get_season(xds_obj)
  bottom <- season_par$bottom
  pw <- season_par$pw
  c(bottom=bottom, pw=pw)
}

#' feature a function
#'
#' @inheritParams update_function_X
#'
#' @returns sum of squared differences
#' @export
update_function_X.amplitude = function(X, xds_obj, feature="amplitude", options=list()){

  bottom <- get_season_bottom(xds_obj)
  bottom <- with(options, modify_vector_X(bottom, bottom_ix, X, bottom_ixX))

  pw <- get_season_pw(xds_obj)
  pw <- with(options, modify_vector_X(pw, pw_ix, X, pw_ixX))

  xds_obj <- change_season(list(pw=pw, bottom=bottom), xds_obj, s=1)

  return(xds_obj)
}

