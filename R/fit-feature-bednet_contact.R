
#' @title Fit bed net coverage
#'
#' @description As part of a dynamical time series
#' analysis, fit the parameter describing
#' bed net coverage
#'
#' @param xds_obj a **`ramp.xds`** model object
#' @param options setup options for the bed net rounds indexing
#'
#' @return a **`ramp.xds`** model object
#'
#' @export
fit_bednet_contact <- function(xds_obj, options=list()){

  options$max_ix = 0
  options <- setup_fitting_indices(xds_obj, "bednet_contact", options)

  if(length(options$bednet_ix)==1){
    lims = get_limits_X(xds_obj, "bednet_contact")
    fitit <- stats::optimize(compute_gof_X, lims, feature="bednet_contact",
                             xds_obj=xds_obj, options=options)
    X <- fitit$minimum
  } else {
    inits = get_init_X(xds_obj, "bednet_contact", options)
    fitit <- stats::optim(inits, compute_gof_X, feature="bednet_contact",
                          options=options, xds_obj=xds_obj)
    X <- fitit$par
  }

  xds_obj <- update_function_X(X, xds_obj, "bednet_contact", options)
  xds_obj <- burnin(xds_obj)
  return(xds_obj)
}

#' Setup indices for bednet coverage
#'
#' @inheritParams setup_fitting_indices
#'
#' @returns indices
#' @export
setup_fitting_indices.bednet_contact = function(xds_obj, feature, options){

  if(with(options, !exists("bednet_ix")))
    options$bednet_ix = which(xds_obj$events_obj$bednet$include)

  options$bednet_ixX = options$max_ix + 1:length(options$bednet_ix)
  options$max_ix = max(options$bednet_ixX)

  return(options)
}

#' feature the bed net coverage function
#'
#' @inheritParams update_function_X
#'
#' @importFrom ramp.control change_bednet_contact_multiround
#'
#' @return a **`ramp.xds`** model object
#' @export
update_function_X.bednet_contact = function(X, xds_obj, feature="bednet_contact", options=list()){
  with(options,{
    contact <- xds_obj$events_obj$bednet$contact
    contact[bednet_ix] <- X[bednet_ixX]
    xds_obj <- change_bednet_contact_multiround(contact, xds_obj)
    return(xds_obj)
})}

#' Get initial X: Bed net coverage
#'
#' @inheritParams get_init_X
#'
#' @return bednet coverage, as [numeric]
#'
#' @export
get_init_X.bednet_contact <- function(xds_obj, feature, options){
  pmax(xds_obj$events_obj$bednet$contact[options$bednet_ix], 0.05)
}

#' Get limits for IRS coverage parameters
#'
#' @inheritParams get_limits_X
#'
#' @return bounds on coverage, as [numeric]
#'
#' @export
get_limits_X.bednet_contact <- function(xds_obj, feature="bednet_contact"){
  return(c(0,1))
}
