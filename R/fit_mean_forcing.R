

#' @title Fit mean forcing
#'
#' @description For a *Pf*PR time series, \eqn{x}, fit
#' the mean forcing parameter
#'
#' @param xds_obj a **`ramp.xds`** model object
#'
#' @return `xds_obj`
#'
#' @export
fit_mean_forcing <- function(xds_obj){

  init <- get_init_X(xds_obj, "mean_forcing")
  lims <- get_limits_X(xds_obj, "mean_forcing")

#  on_boundary=TRUE
#  while(on_boundary == TRUE){
#    on_boundary = FALSE
    fitit <- stats::optimize(compute_gof_X, lims,
                             update = "mean_forcing",
                             xds_obj=xds_obj, ix=1)
#    browser()
#    if(abs(fitit$minimum-L/5) < 1e-3){
#      on_boundary = TRUE
#      L=L/24
#    }
#    if(abs(fitit$minimum-5*L) < 1e-3){
#      on_boundary = TRUE
#      L=L*24
#    }
#  }
  X <- fitit$minimum
  xds_obj <- update_function_X(X, xds_obj, "mean_forcing")
  xds_obj <- burnin(xds_obj, 10)
  return(xds_obj)
}

#' Get Initial Values for Parameters
#'
#' @inheritParams get_limits_X
#'
#' @return a vector
#' @export
get_limits_X.mean_forcing <- function(xds_obj, update="mean_forcing"){
  L <- get_init_X(xds_obj, "mean_forcing")
  return(c(L/5, 5*L))
}

#' Get Initial Values for Parameters
#'
#' @inheritParams get_init_X
#'
#' @return a vector
#' @export
get_init_X.mean_forcing<- function(xds_obj, update="mean_forcing", ix=1){
  get_mean_forcing(xds_obj)
}


#' Update Mean Forcing
#'
#' @description Update the mean value
#' that forces a xds_obj. This dispatches
#' on class(xds_obj$forced_by)
#'
#' @inheritParams update_function_X
#'
#' @returns sum of squared differences
#' @export
update_function_X.mean_forcing = function(X, xds_obj, update="mean_forcing", ix=1){
  avg   <- get_mean_forcing(xds_obj)
  avg   <- modify_vector_X(X, ix, avg)
  xds_obj <- set_mean_forcing(avg, xds_obj, 1)
  return(xds_obj)
}

