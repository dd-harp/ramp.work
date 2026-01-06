
#' @title Fit Bednet Shock
#'
#' @description As part of a dynamical time series
#' analysis, fit the parameter(s) describing
#' the scale of bednet effect sizes.
#'
#' This should not be used
#'
#' @param xds_obj a **`ramp.xds`** model object
#' @param options setup options for the bednet rounds indexing
#'
#' @return a **`ramp.xds`** model object
#'
#' @export
fit_bednet_shock <- function(xds_obj, options=list()){
  stopifnot(class(xds_obj$frame) == "eir")

  options$max_ix = 0
  options <- setup_fitting_indices(xds_obj, "bednet_shock", options)


  inits = get_init_X(xds_obj, "bednet_shock", options)

  fitit <- stats::optim(inits, compute_gof_X, feature="bednet_shock",
                          options=options, xds_obj=xds_obj)
  X <- fitit$par

  xds_obj <- update_function_X(X, xds_obj, "bednet_shock", options)
  xds_obj <- burnin(xds_obj)
  return(xds_obj)
}

#' Setup indices for bednet shock
#'
#' @inheritParams setup_fitting_indices
#'
#' @returns indices
#' @export
setup_fitting_indices.bednet_shock = function(xds_obj, feature, options){

  if(with(options, !exists("bednet_ix")))
    options$bednet_ix = 1:xds_obj$events_obj$bednet$N

  L = length(options$bednet_ix)
  options$bednet_L = L
  options$bednet_ixX = options$max_ix + 1:L

  options$max_ix = options$max_ix + 3*L

  return(options)
}

#' feature the bednet shock function
#'
#' @inheritParams update_function_X
#'
#' @importFrom ramp.control change_bednet_shock_multiround
#'
#' @returns sum of squared differences
#' @export
update_function_X.bednet_shock = function(X, xds_obj, feature="bednet_shock", options=list()){

  with(options,{
    shock <- xds_obj$events_obj$bednet$shock
    shock[bednet_ix] <- sigX(X[bednet_ixX])

    xds_obj$events_obj$bednet$d_50[bednet_ix] <- X[bednet_ixX+bednet_L]^2
    xds_obj$events_obj$bednet$d_shape[bednet_ix] <- sigX(X[bednet_ixX+2*bednet_L])

    xds_obj <- change_bednet_shock_multiround(xds_obj, shock)
    return(xds_obj)
  })}

#' Get initial X: bednet shock
#'
#' @inheritParams get_init_X
#' @return bednet shock levels
#' @export
get_init_X.bednet_shock <- function(xds_obj, feature, options){
  with(options,{
    shock = xds_obj$events_obj$bednet$shock[bednet_ix]
    d_50 = xds_obj$events_obj$bednet$d_50[bednet_ix]
    d_shape = xds_obj$events_obj$bednet$d_shape[bednet_ix]
  return(c(sigXinv(shock), sqrt(d_50), sigXinv(d_shape)))
})}

#' Get Initial Values for Parameters
#'
#' @inheritParams get_limits_X
#'
#' @return a vector
#' @export
get_limits_X.bednet_shock <- function(xds_obj, feature="bednet_shock"){
  return(c(0,1))
}
