
#' @title IRS Effects: Fit Shape
#'
#' @description As part of a dynamical time series
#' analysis, fit the parameter(s) `d_shape` describing
#' the shape of the response timeline
#'
#' @param xds_obj a **`ramp.xds`** model object
#' @param options setup options for the irs rounds indexing
#'
#' @return a **`ramp.xds`** model object
#'
#' @export
fit_irs_dshape <- function(xds_obj, options=list()){

  options$max_ix = 0
  options <- setup_fitting_indices(xds_obj, "irs_dshape", options)

  if(length(options$irs_ix)==1){
    lims = get_limits_X(xds_obj, "irs_dshape")
    fitit <- stats::optimize(compute_gof_X, lims, feature="irs_dshape",
                             xds_obj=xds_obj, options=options)
    X <- fitit$minimum
  } else {
    inits = get_init_X(xds_obj, "irs_dshape", options)
    fitit <- stats::optim(inits, compute_gof_X, feature="irs_dshape",
                          options=options, xds_obj=xds_obj)
    X <- fitit$par
  }

  xds_obj <- update_function_X(X, xds_obj, "irs_dshape", options)
  xds_obj <- burnin(xds_obj)
  return(xds_obj)
}

#' Setup indices for irs dshape
#'
#' @inheritParams setup_fitting_indices
#'
#' @returns indices
#' @export
setup_fitting_indices.irs_dshape = function(xds_obj, feature, options){

  if(with(options, !exists("irs_ix")))
    options$irs_ix = 1:xds_obj$events_obj$irs$N

  options$irs_ixX = options$max_ix + 1:length(options$irs_ix)
  options$max_ix = max(options$irs_ixX)

  return(options)
}

#' feature the irs dshape function
#'
#' @inheritParams update_function_X
#'
#' @importFrom ramp.control change_irs_shock_multiround
#'
#' @returns sum of squared differences
#' @export
update_function_X.irs_dshape = function(X, xds_obj, feature="irs_dshape", options=list()){
  with(options,{
    dshape <- xds_obj$events_obj$irs$d_shape
    dshape[irs_ix] <- X2dshape(X[irs_ixX])
    xds_obj <- change_irs_shock_multiround(xds_obj, dshape)
    return(xds_obj)
  })}

#' @title `X2bottom`
#'
#' @description Ensure the `d_shape`
#' parameter is properly bounded
#'
#' @param x a number
#'
#' @returns a number
#' @export
X2dshape= function(x){
  exp(x)/(1+exp(x))
}


#' Get initial X: IRS dshape
#'
#' @inheritParams get_init_X
#' @return IRS dshape levels
#' @export
get_init_X.irs_dshape <- function(xds_obj, feature, options){
  inits <- pmax(xds_obj$events_obj$irs$d_shape[options$irs_ix], 0.1)
  return(inits)
}

#' Get Initial Values for Parameters
#'
#' @inheritParams get_limits_X
#'
#' @return a vector
#' @export
get_limits_X.irs_dshape <- function(xds_obj, feature="irs_dshape"){
  return(c(0,1))
}
