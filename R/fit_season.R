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

  inits = get_init_X(xds_obj, "season")

  fitit <- stats::optim(inits, compute_gof_X,
                        xds_obj=xds_obj, ix=1,
                        update="season")

  X <- fitit$par
  xds_obj <- update_function_X(fitit$par, xds_obj, update = "season")
  xds_obj <- burnin(xds_obj)
  return(xds_obj)
}


#' Get Initial Values for Parameters
#'
#' @inheritParams get_init_X
#'
#' @return a vector
#' @export
get_init_X.season <- function(xds_obj, update="season", ix=1){
  season_par <- get_season(xds_obj)
  phase <- season_par$phase[ix]
  bottom <- season_par$bottom[ix]
  pw <- season_par$pw[ix]
  c(phase=phase, bottom=bottom, pw=pw)
}

#' @title Update `F_season`
#'
#' @description This updates three shape parameters
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
update_function_X.season = function(X, xds_obj, update="season", ix=1){

  stopifnot(length(X)%%3 == 0)
  l = length(X)/3
  X_phase = X[1:l]
  X_bottom = X[1:l + l]
  X_pw = X[1:l + 2*l]

  phase <- get_season_phase(xds_obj)
  phase <- modify_vector_X(X_phase, ix, phase)
  xds_obj <- set_season_phase(phase, xds_obj, s=1)

  bottom <- get_season_bottom(xds_obj)
  bottom <- modify_vector_X(X_bottom, ix, bottom)
  xds_obj <- set_season_bottom(bottom, xds_obj, s=1)

  pw <- get_season_pw(xds_obj)
  pw <- modify_vector_X(X_pw, ix, phase)
  xds_obj <- set_season_pw(pw, xds_obj, s=1)

  xds_obj <- update_F_season(xds_obj)

  return(xds_obj)
}



