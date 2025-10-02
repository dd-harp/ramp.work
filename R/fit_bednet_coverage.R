
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
fit_bednet_coverage <- function(xds_obj, options=list()){

  options$max_ix = 0
  options <- setup_fitting_indices(xds_obj, "bednet_coverage", options)


  bednet_coverage = get_init_X(xds_obj, "bednet_coverage")
  lims = get_limits_X(xds_obj, "bednet_coverage")

  fitit <- stats::optimize(compute_gof_X, lims, feature="bednet_coverage",
                           xds_obj=xds_obj, options=options)

  xds_obj <- update_function_X(fitit$minimum, xds_obj, "bednet_coverage", options)
  xds_obj <- burnin(xds_obj)
  return(xds_obj)
}

#' Setup indices for bednet coverage
#'
#' @inheritParams setup_fitting_indices
#'
#' @returns indices
#' @export
setup_fitting_indices.bednet_coverage = function(xds_obj, feature, options){
  if(length(options)==0)
    options$bednet_ix =c(1:length(xds_obj$bednet_obj$cover_obj$nRounds))

  stopifnot(!is.null(options$bednet_ix))

  options$bednet_ixX = options$max_ix + 1:length(options$bednet_ix)
  options$max_ix = max(options$bednet_ixX)

  return(options)
}

#' feature the bed net coverage function
#'
#' @inheritParams update_function_X
#'
#' @importFrom ramp.control get_bednet_coverage change_bednet_coverage make_F_cover_bednet
#'
#' @return a **`ramp.xds`** model object
#' @export
update_function_X.bednet_coverage = function(X, xds_obj, feature="bednet_coverage", options=list()){

  bednet_coverage <- get_bednet_coverage(xds_obj)
  bednet_coverage <- with(options, modify_vector_X(bednet_ix, bednet_coverage, X, bednet_ixX))
  xds_obj <- change_bednet_coverage(bednet_coverage, xds_obj)

  return(xds_obj)
}

#' Get initial X: Bed net coverage
#'
#' @inheritParams get_init_X
#'
#' @return bednet coverage, as [numeric]
#'
#' @export
get_init_X.bednet_coverage <- function(xds_obj, feature, options){
  return(get_bednet_coverage(xds_obj)[options$bednet_ix])
}

#' Get limits for IRS coverage parameters
#'
#' @inheritParams get_limits_X
#'
#' @return bounds on coverage, as [numeric]
#'
#' @export
get_limits_X.bednet_coverage <- function(xds_obj, feature="bednet_coverage"){
  return(c(0,1))
}
