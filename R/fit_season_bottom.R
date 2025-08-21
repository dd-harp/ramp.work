
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

  bottom = get_init_X(xds_obj, "bottom")
  lims = get_limits_X(xds_obj, "bottom")

  fitit <- stats::optimize(compute_gof_X, c(0,10),
                           xds_obj=xds_obj, ix=1, update = "bottom")

  X <- fitit$minimum
  xds_obj <- update_function_X(X, xds_obj, update="bottom")
  xds_obj <- burnin(xds_obj)
  return(xds_obj)
}

#' Get Initial Values for Parameters
#'
#' @inheritParams get_limits_X
#'
#' @return a vector
#' @export
get_limits_X.bottom <- function(xds_obj, update="bottom"){
  return(c(0,365))
}

#' Get Initial Values for Parameters
#'
#' @inheritParams get_init_X
#'
#' @return a vector
#' @export
get_init_X.bottom <- function(xds_obj, update="bottom", ix=1){
  return(get_season(xds_obj)$bottom[ix])
}

#' Update a function
#'
#' @inheritParams update_function_X
#'
#' @returns sum of squared differences
#' @export
update_function_X.bottom = function(X, xds_obj, update="bottom", ix=1){
  bottom <- get_season_bottom(xds_obj)
  bottom <- modify_vector_X(X, ix, bottom)
  xds_obj <- set_season_bottom(bottom, xds_obj, s=1)
  xds_obj <- update_F_season(xds_obj)

  return(xds_obj)
}

