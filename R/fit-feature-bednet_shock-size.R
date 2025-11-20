
#' @title Fit bednet shock_size
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
fit_bednet_shock_size <- function(xds_obj, options=list()){
  stopifnot(class(xds_obj$frame) == "eir")

  options$max_ix = 0
  options <- setup_fitting_indices(xds_obj, "bednet_shock_size", options)

  if(length(options$bednet_ix)==1){
    lims = get_limits_X(xds_obj, "bednet_shock_size")
    fitit <- stats::optimize(compute_gof_X, lims, feature="bednet_shock_size",
                             xds_obj=xds_obj, options=options)
    X <- fitit$minimum
  } else {
    inits = get_init_X(xds_obj, "bednet_shock_size", options)
    fitit <- stats::optim(inits, compute_gof_X, feature="bednet_shock_size",
                          options=options, xds_obj=xds_obj)
    X <- fitit$par
  }

  xds_obj <- update_function_X(X, xds_obj, "bednet_shock_size", options)
  xds_obj <- burnin(xds_obj)
  return(xds_obj)
}

#' Setup indices for bednet shock_size
#'
#' @inheritParams setup_fitting_indices
#'
#' @returns indices
#' @export
setup_fitting_indices.bednet_shock_size = function(xds_obj, feature, options){

  if(with(options, !exists("bednet_ix")))
    options$bednet_ix = 1:xds_obj$events_obj$bednet$N

  options$bednet_ixX = options$max_ix + 1:length(options$bednet_ix)
  options$max_ix = max(options$bednet_ixX)

  return(options)
}

#' feature the bednet shock_size function
#'
#' @inheritParams update_function_X
#'
#' @importFrom ramp.control change_bednet_shock_multiround
#'
#' @returns sum of squared differences
#' @export
update_function_X.bednet_shock_size = function(X, xds_obj, feature="bednet_shock_size", options=list()){
  with(options,{
    size <- xds_obj$events_obj$bednet$shock
    size[bednet_ix] <- sigX(X[bednet_ixX])
    xds_obj <- change_bednet_shock_multiround(xds_obj, size)
    return(xds_obj)
  })}

#' Get initial X: bednet shock_size
#'
#' @inheritParams get_init_X
#' @return bednet shock_size levels
#' @export
get_init_X.bednet_shock_size <- function(xds_obj, feature, options){
  last <- xds_obj$events_obj$bednet$shock[options$bednet_ix]
  last <- pmax(last, 1e-3)
  inits <- log(last/(1-last))
  return(inits)
}

#' Get Initial Values for Parameters
#'
#' @inheritParams get_limits_X
#'
#' @return a vector
#' @export
get_limits_X.bednet_shock_size <- function(xds_obj, feature="bednet_shock_size"){
  return(c(0,1))
}
