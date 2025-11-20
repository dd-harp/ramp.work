
#' @title Fit bednet dshape
#'
#' @description As part of a dynamical time series
#' analysis, fit the parameter(s) describing
#' the scale of bednet effect dshapes.
#'
#' This should not be used
#'
#' @param xds_obj a **`ramp.xds`** model object
#' @param options setup options for the bednet rounds indexing
#'
#' @return a **`ramp.xds`** model object
#'
#' @export
fit_bednet_dshape <- function(xds_obj, options=list()){

  options$max_ix = 0
  options <- setup_fitting_indices(xds_obj, "bednet_dshape", options)

  if(length(options$bednet_ix)==1){
    lims = get_limits_X(xds_obj, "bednet_dshape")
    fitit <- stats::optimize(compute_gof_X, lims, feature="bednet_dshape",
                             xds_obj=xds_obj, options=options)
    X <- fitit$minimum
  } else {
    inits = get_init_X(xds_obj, "bednet_dshape", options)
    fitit <- stats::optim(inits, compute_gof_X, feature="bednet_dshape",
                          options=options, xds_obj=xds_obj)
    X <- fitit$par
  }

  xds_obj <- update_function_X(X, xds_obj, "bednet_dshape", options)
  xds_obj <- burnin(xds_obj)
  return(xds_obj)
}

#' Setup indices for bednet dshape
#'
#' @inheritParams setup_fitting_indices
#'
#' @returns indices
#' @export
setup_fitting_indices.bednet_dshape = function(xds_obj, feature, options){

  if(with(options, !exists("bednet_ix")))
    options$bednet_ix = 1:xds_obj$events_obj$bednet$N

  options$bednet_ixX = options$max_ix + 1:length(options$bednet_ix)
  options$max_ix = max(options$bednet_ixX)

  return(options)
}

#' feature the bednet dshape function
#'
#' @inheritParams update_function_X
#'
#' @importFrom ramp.control change_bednet_shock_multiround
#'
#' @returns sum of squared differences
#' @export
update_function_X.bednet_dshape = function(X, xds_obj, feature="bednet_dshape", options=list()){
  with(options,{
    dshape <- xds_obj$events_obj$bednet$d_shape
    dshape[bednet_ix] <- X2dshape(X[bednet_ixX])
    xds_obj <- change_bednet_shock_multiround(xds_obj, dshape)
    return(xds_obj)
  })}

#' Get initial X: bednet dshape
#'
#' @inheritParams get_init_X
#' @return bednet dshape levels
#' @export
get_init_X.bednet_dshape <- function(xds_obj, feature, options){
  inits <- pmax(xds_obj$events_obj$bednet$d_shape[options$bednet_ix], 0.1)
  return(inits)
}

#' Get Initial Values for Parameters
#'
#' @inheritParams get_limits_X
#'
#' @return a vector
#' @export
get_limits_X.bednet_dshape <- function(xds_obj, feature="bednet_dshape"){
  return(c(0,1))
}
