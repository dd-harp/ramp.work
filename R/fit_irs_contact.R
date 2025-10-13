
#' @title Fit irs coverage
#'
#' @description As part of a dynamical time series
#' analysis, fit the parameter(s) describing
#' irs coverage
#'
#' @param xds_obj a **`ramp.xds`** model object
#' @param options setup options for the irs rounds indexing
#'
#' @return a **`ramp.xds`** model object
#'
#' @export
fit_irs_coverage <- function(xds_obj, options=list()){

  options$max_ix = 0
  options <- setup_fitting_indices(xds_obj, "irs_coverage", options)

  irs_coverage = get_init_X(xds_obj, "irs_coverage")

  lims = get_limits_X(xds_obj, "irs_coverage")

  fitit <- stats::optimize(compute_gof_X, lims, feature = "irs_coverage",
                           xds_obj=xds_obj, options=options)

  xds_obj <- update_function_X(fitit$minimum, xds_obj, "irs_coverage", options)
  xds_obj <- burnin(xds_obj)
  return(xds_obj)
}

#' Setup indices for irs coverage
#'
#' @inheritParams setup_fitting_indices
#'
#' @returns indices
#' @export
setup_fitting_indices.irs_coverage = function(xds_obj, feature, options){

  if(with(options, !exists("irs_ix")))
    options$irs_ix = which(xds_obj$events_obj$irs$include)

  options$irs_ixX = options$max_ix + 1:length(options$irs_ix)
  options$max_ix = max(options$irs_ixX)

  return(options)
}

#' feature the irs coverage function
#'
#' @inheritParams update_function_X
#'
#' @importFrom ramp.control get_irs_coverage change_irs_coverage make_F_cover_irs
#' @returns sum of squared differences
#' @export
update_function_X.irs_coverage = function(X, xds_obj, feature="irs_coverage", options=list()){
  with(options,{
    irs_contact <- xds_obj$events_obj$irs$contact
    irs_contact[irs_ix] <- X[irs_ixX]
    xds_obj <- change_irs_contact_multiround(irs_contact, xds_obj)
  return(xds_obj)
})}

#' Get initial X: IRS coverage
#'
#' @inheritParams get_init_X
#' @return IRS coverage levels
#' @export
get_init_X.irs_coverage <- function(xds_obj, feature, options){
 return(get_irs_coverage(xds_obj)[options$irs_ix])
}

#' Get Initial Values for Parameters
#'
#' @inheritParams get_limits_X
#'
#' @return a vector
#' @export
get_limits_X.irs_coverage <- function(xds_obj, feature="irs_coverage"){
  return(c(0,1))
}
