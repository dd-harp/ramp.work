
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

  pw = get_init_X(xds_obj, "pw")
  lims = get_limits_X(xds_obj, "pw")

  fitit <- stats::optimize(compute_gof_X, lims,
                           xds_obj=xds_obj, ix=1, update = "pw")

  X <- fitit$minimum
  xds_obj <- update_function_X(X, xds_obj, update="pw")
  xds_obj <- burnin(xds_obj)
  return(xds_obj)
}

#' Get Initial Values for Parameters
#'
#' @inheritParams get_limits_X
#'
#' @return a vector
#' @export
get_limits_X.pw <- function(xds_obj, update="pw"){
  return(c(0,10))
}

#' Get Initial Values for Parameters
#'
#' @inheritParams get_init_X
#'
#' @return a vector
#' @export
get_init_X.pw <- function(xds_obj, update="pw", ix=1){
  return(get_season(xds_obj)$pw)
}

#' Update a function
#'
#' @inheritParams update_function_X
#'
#' @returns sum of squared differences
#' @export
update_function_X.pw = function(X, xds_obj, update="pw", ix=1){

  pw <- get_season_pw(xds_obj)
  pw <- modify_vector_X(X, ix, pw)
  xds_obj <- set_season_pw(pw, xds_obj, 1)
  xds_obj <- update_F_season(xds_obj)

  return(xds_obj)
}
