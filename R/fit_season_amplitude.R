
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

  inits = get_init_X(xds_obj, "amplitude")

  fitit <- stats::optim(inits, compute_gof_X,
                        xds_obj=xds_obj, ix=1,
                        update="amplitude")

  X <- fitit$par
  xds_obj <- update_function_X(X, xds_obj, update = "amplitude")
  xds_obj <- burnin(xds_obj)
  return(xds_obj)
}

#' Get Initial Values for Parameters
#'
#' @inheritParams get_init_X
#'
#' @return a vector
#' @export
get_init_X.amplitude <- function(xds_obj, update="amplitude", ix=1){
  season_par <- get_season(xds_obj)
  bottom <- season_par$bottom[ix]
  pw <- season_par$pw[ix]
  c(bottom=bottom, pw=pw)
}

#' Update a function
#'
#' @inheritParams update_function_X
#'
#' @returns sum of squared differences
#' @export
update_function_X.amplitude = function(X, xds_obj, update="amplitude", ix=1){
  stopifnot(length(X) == 2*length(ix))
  l = length(ix)
  X_bottom = X[1:l]
  X_pw = X[1:l+l]

  bottom <- get_season_bottom(xds_obj)
  bottom <- modify_vector_X(X_bottom, ix, bottom)
  xds_obj <- set_season_bottom(bottom, xds_obj, s=1)

  pw <- get_season_pw(xds_obj)
  pw <- modify_vector_X(X_pw, ix, pw)
  xds_obj <- set_season_pw(pw, xds_obj, s=1)

  xds_obj <- update_F_season(xds_obj)
  return(xds_obj)
}

