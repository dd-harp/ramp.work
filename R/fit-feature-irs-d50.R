
#' @title Fit IRS shock_d50
#'
#' @description As part of a dynamical time series
#' analysis, fit the parameter(s) describing
#' the scale of IRS effect d50s.
#'
#' This should not be used
#'
#' @param xds_obj a **`ramp.xds`** model object
#' @param options setup options for the irs rounds indexing
#'
#' @return a **`ramp.xds`** model object
#'
#' @export
fit_irs_d50 <- function(xds_obj, options=list()){
  stopifnot(class(xds_obj$frame) == "eir")

  options$max_ix = 0
  options <- setup_fitting_indices(xds_obj, "irs_d50", options)

  if(length(options$irs_ix)==1){
    lims = get_limits_X(xds_obj, "irs_d50")
    fitit <- stats::optimize(compute_gof_X, lims, feature="irs_d50",
                             xds_obj=xds_obj, options=options)
    X <- fitit$minimum
  } else {
    inits = get_init_X(xds_obj, "irs_d50", options)
    fitit <- stats::optim(inits, compute_gof_X, feature="irs_d50",
                          options=options, xds_obj=xds_obj)
    X <- fitit$par
  }

  xds_obj <- update_function_X(X, xds_obj, "irs_d50", options)
  xds_obj <- burnin(xds_obj)
  return(xds_obj)
}

#' Setup indices for irs shock_d50
#'
#' @inheritParams setup_fitting_indices
#'
#' @returns indices
#' @export
setup_fitting_indices.irs_d50 = function(xds_obj, feature, options){

  if(with(options, !exists("irs_ix")))
    options$irs_ix = 1:xds_obj$events_obj$irs$N

  options$irs_ixX = options$max_ix + 1:length(options$irs_ix)
  options$max_ix = max(options$irs_ixX)

  return(options)
}

#' feature the irs shock_d50 function
#'
#' @inheritParams update_function_X
#'
#' @importFrom ramp.control change_irs_shock_multiround
#'
#' @returns sum of squared differences
#' @export
update_function_X.irs_d50 = function(X, xds_obj, feature="irs_d50", options=list()){
  with(options,{
    shock_d50 <- xds_obj$events_obj$irs$d_50
    shock_d50[irs_ix] <- X2d50(X[irs_ixX])
    xds_obj <- change_irs_shock_multiround(xds_obj, shock_d50)
    return(xds_obj)
  })}


#' Get initial X: IRS shock_d50
#'
#' @inheritParams get_init_X
#' @return IRS shock_d50 levels
#' @export
get_init_X.irs_d50 <- function(xds_obj, feature, options){
  inits <- pmax(xds_obj$events_obj$irs$d_50[options$irs_ix], 0.1)
  return(inits)
}

#' Get Initial Values for Parameters
#'
#' @inheritParams get_limits_X
#'
#' @return a vector
#' @export
get_limits_X.irs_d50 <- function(xds_obj, feature="irs_d50"){
  return(c(0,1))
}
