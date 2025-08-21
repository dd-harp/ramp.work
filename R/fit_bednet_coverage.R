
#' @title Fit bed net coverage
#'
#' @description As part of a dynamical time series
#' analysis, fit the parameter describing
#' bed net coverage
#'
#' @param xds_obj a **`ramp.xds`** model object
#'
#' @return a **`ramp.xds`** model object
#'
#' @export
fit_bednet_coverage <- function(xds_obj){

  bednet_coverage = get_init_X(xds_obj, "bednet_coverage")
  lims = get_limits_X(xds_obj, "bednet_coverage")

  fitit <- stats::optimize(compute_gof_X, lims,
                           xds_obj=xds_obj, ix=1, update = "bednet_coverage")

  xds_obj <- update_function_X(fitit$minimum, xds_obj, update = "bednet_coverage")
  xds_obj <- burnin(xds_obj)
  return(xds_obj)
}

#' Update the bed net coverage function
#'
#' @inheritParams update_function_X
#'
#' @importFrom ramp.control get_bednet_coverage set_bednet_coverage setup_F_cover_bednet
#'
#' @return a **`ramp.xds`** model object
#' @export
update_function_X.bednet_coverage = function(X, xds_obj, update="bednet_coverage", ix=1){

  bednet_coverage <- get_bednet_coverage(xds_obj)
  bednet_coverage <- modify_vector_X(X, ix, bednet_coverage)
  xds_obj <- set_bednet_coverage(bednet_coverage, xds_obj)
  xds_obj$bednet$coverage_mod <- setup_F_cover_bednet(xds_obj$bednet$coverage_mod)

  return(xds_obj)
}

#' Get initial X: Bed net coverage
#'
#' @inheritParams get_init_X
#'
#' @return bednet coverage, as [numeric]
#'
#' @export
get_init_X.bednet_coverage <- function(xds_obj, update="bednet_coverage", ix=1){
  return(get_bednet_coverage(xds_obj)[ix])
}

#' Get limits for IRS coverage parameters
#'
#' @inheritParams get_limits_X
#'
#' @return bounds on coverage, as [numeric]
#'
#' @export
get_limits_X.bednet_coverage <- function(xds_obj, update="bednet_coverage"){
  return(c(0,1))
}
