

#' @title Fit mean forcing
#'
#' @description Fit the mean
#' forcing parameter. Functions
#' called by `fit_mean_forcing` dispatch
#' on `class(xds_obj$forced_by)`
#'
#' + `eir` updates the mean daily EIR
#' + `Lambda` updates the mean daily emergence rate
#'
#' The method is designed for fitting a model to
#' observed values of *Pf*PR in a time series,
#' \eqn{x_t.}
#'
#' @note The function parameter `options` is not used.
#'
#' @param xds_obj an **`xds`** model object
#'
#' @return an **`xds`** model object
#'
#' @export
fit_mean_forcing <- function(xds_obj){

  options=list()
  options$max_ix = 0
  options <- setup_fitting_indices(xds_obj, "mean_forcing", options)

  init <- get_init_X(xds_obj, "mean_forcing")
  lims <- get_limits_X(xds_obj, "mean_forcing")

  fitit <- stats::optimize(compute_gof_X, lims,
                             feature = "mean_forcing",
                             xds_obj=xds_obj, options=options)

  X <- fitit$minimum
  xds_obj <- update_function_X(X, xds_obj, "mean_forcing", options)
  xds_obj <- burnin(xds_obj)
  return(xds_obj)
}

#' Indices for Mean Forcing
#'
#' @inheritParams setup_fitting_indices
#'
#' @returns options
#'
#' @export
#'
setup_fitting_indices.mean_forcing = function(xds_obj, feature, options){

  options$avg_ix = 1
  options$avg_ixX = options$max_ix + 1:length(options$avg_ix)
  options$max_ix = max(options$avg_ixX)

  return(options)
}

#' Get Initial Values for Parameters
#'
#' @inheritParams get_limits_X
#'
#' @return a vector
#' @export
get_limits_X.mean_forcing <- function(xds_obj, feature="mean_forcing"){
  L <- get_init_X(xds_obj, "mean_forcing")
  return(c(L/5, 5*L))
}

#' Get Initial Values for Parameters
#'
#' @inheritParams get_init_X
#'
#' @return a vector
#' @export
get_init_X.mean_forcing<- function(xds_obj, feature, options=list()){
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
update_function_X.mean_forcing = function(X, xds_obj, feature, options){
  avg   <- get_mean_forcing(xds_obj)
  avg   <- with(options, modify_vector_X(avg, avg_ix, X, avg_ixX))
  xds_obj <- change_mean_forcing(avg, xds_obj, 1)
  return(xds_obj)
}

