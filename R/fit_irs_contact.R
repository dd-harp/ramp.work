
#' @title Fit irs contact
#'
#' @description As part of a dynamical time series
#' analysis, fit the parameter(s) describing
#' irs contact
#'
#' @param xds_obj a **`ramp.xds`** model object
#' @param options setup options for the irs rounds indexing
#'
#' @return a **`ramp.xds`** model object
#'
#' @export
fit_irs_contact <- function(xds_obj, options=list()){

  options$max_ix = 0
  options <- setup_fitting_indices(xds_obj, "irs_contact", options)

  if(length(options$irs_ix)==1){
    lims = get_limits_X(xds_obj, "irs_contact")
    fitit <- stats::optimize(compute_gof_X, lims, feature="irs_contact",
                             xds_obj=xds_obj, options=options)
    X <- fitit$minimum
  } else {
    inits = get_init_X(xds_obj, "irs_contact", options)
    fitit <- stats::optim(inits, compute_gof_X, feature="irs_contact",
                          options=options, xds_obj=xds_obj)
    X <- fitit$par
  }

  xds_obj <- update_function_X(X, xds_obj, "irs_contact", options)
  xds_obj <- burnin(xds_obj)
  return(xds_obj)
}

#' Setup indices for irs contact
#'
#' @inheritParams setup_fitting_indices
#'
#' @returns indices
#' @export
setup_fitting_indices.irs_contact = function(xds_obj, feature, options){

  if(with(options, !exists("irs_ix")))
    options$irs_ix = 1:xds_obj$events_obj$irs$N

  options$irs_ixX = options$max_ix + 1:length(options$irs_ix)
  options$max_ix = max(options$irs_ixX)

  return(options)
}

#' feature the irs contact function
#'
#' @inheritParams update_function_X
#'
#' @importFrom ramp.control change_irs_contact_multiround
#'
#' @returns sum of squared differences
#' @export
update_function_X.irs_contact = function(X, xds_obj, feature="irs_contact", options=list()){
  with(options,{
    irs_contact <- xds_obj$events_obj$irs$contact
    irs_contact[irs_ix] <- X[irs_ixX]
    xds_obj <- change_irs_contact_multiround(irs_contact, xds_obj)
  return(xds_obj)
})}

#' Get initial X: IRS contact
#'
#' @inheritParams get_init_X
#' @return IRS contact levels
#' @export
get_init_X.irs_contact <- function(xds_obj, feature, options){
 inits <- pmax(xds_obj$events_obj$irs$contact[options$irs_ix], 0.1)
 return(inits)
}

#' Get Initial Values for Parameters
#'
#' @inheritParams get_limits_X
#'
#' @return a vector
#' @export
get_limits_X.irs_contact <- function(xds_obj, feature="irs_contact"){
  return(c(0,1))
}
