
#' @title Fit IRS Shock
#'
#' @description As part of a dynamical time series
#' analysis, fit the parameters describing
#' the response timeline to IRS. This fits three parameters:
#'
#' + `size` controls the maximum effect size of the shock
#' + `d_50` days after the shock when the effect size reaches half of the maximum
#' + `d_shape` a shape parameter
#'
#' @param xds_obj a **`ramp.xds`** model object
#' @param options setup options for the irs rounds indexing
#'
#' @return a **`ramp.xds`** model object
#'
#' @export
fit_irs_shock <- function(xds_obj, options=list()){
  stopifnot(class(xds_obj$frame) == "eir")

  options$max_ix = 0
  options <- setup_fitting_indices(xds_obj, "irs_shock", options)

  inits = get_init_X(xds_obj, "irs_shock", options)
  fitit <- stats::optim(inits, compute_gof_X, feature="irs_shock",
                          options=options, xds_obj=xds_obj)
  X <- fitit$par

  xds_obj <- update_function_X(X, xds_obj, "irs_shock", options)
  xds_obj <- burnin(xds_obj)
  return(xds_obj)
}

#' Setup indices for irs shock
#'
#' @inheritParams setup_fitting_indices
#'
#' @returns indices
#' @export
setup_fitting_indices.irs_shock = function(xds_obj, feature, options){

  if(with(options, !exists("irs_ix")))
    options$irs_ix = 1:xds_obj$events_obj$irs$N

  L = length(options$irs_ix)
  options$irs_L = L
  options$irs_ixX = options$max_ix + 1:L

  options$max_ix = options$max_ix + 3*L

  return(options)
}

#' feature the irs shock function
#'
#' @inheritParams update_function_X
#'
#' @importFrom ramp.control change_irs_shock_multiround
#'
#' @returns sum of squared differences
#' @export
update_function_X.irs_shock = function(X, xds_obj, feature="irs_shock", options=list()){
  with(options,{
    shock <- xds_obj$events_obj$irs$shock
    shock[irs_ix] <- sigX(X[irs_ixX])

    xds_obj$events$irs$d_50[irs_ix] <- X[irs_ixX+irs_L]^2
    xds_obj$events$irs$d_shape[irs_ix] <- sigX(X[irs_ixX+irs_L*2])

    xds_obj <- change_irs_shock_multiround(xds_obj, shock)

    return(xds_obj)
  })}

#' Get initial X: IRS shock
#'
#' @inheritParams get_init_X
#' @return IRS shock levels
#' @export
get_init_X.irs_shock <- function(xds_obj, feature, options){

  shock = xds_obj$events_obj$irs$shock[options$irs_ix]
  d_50 = with(options, xds_obj$events_obj$irs$d_50[irs_ix])
  d_shape = with(options, xds_obj$events_obj$irs$d_shape[irs_ix])

  return(c(sigXinv(shock), sqrt(d_50), sigXinv(d_shape)))
}

