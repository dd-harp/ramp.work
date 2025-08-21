
#' @title Fit irs coverage
#'
#' @description As part of a dynamical time series
#' analysis, fit the parameter(s) describing
#' irs coverage
#'
#' @param xds_obj a **`ramp.xds`** model object
#'
#' @return a **`ramp.xds`** model object
#'
#' @export
fit_irs_coverage <- function(xds_obj){

  irs_coverage = get_init_X(xds_obj, "irs_coverage")

  lims = get_limits_X(xds_obj, "irs_coverage")

  fitit <- stats::optimize(compute_gof_X, lims,
                           xds_obj=xds_obj, ix=1, update = "irs_coverage")

  xds_obj <- update_function_X(fitit$minimum, xds_obj, update = "irs_coverage")
  xds_obj <- burnin(xds_obj)
  return(xds_obj)
}

#' Update the irs coverage function
#'
#' @inheritParams update_function_X
#'
#' @importFrom ramp.control get_irs_coverage set_irs_coverage setup_F_cover_irs
#' @returns sum of squared differences
#' @export
update_function_X.irs_coverage = function(X, xds_obj, update="irs_coverage", ix=1){

  irs_coverage <- get_irs_coverage(xds_obj)
  irs_coverage <- modify_vector_X(X, ix, irs_coverage)
  xds_obj <- set_irs_coverage(irs_coverage, xds_obj)
  xds_obj$irs$coverage_mod <- setup_F_cover_irs(xds_obj$irs$coverage_mod)

  return(xds_obj)
}

#' Get initial X: IRS coverage
#'
#' @inheritParams get_init_X
#' @return IRS coverage levels
#' @export
get_init_X.irs_coverage <- function(xds_obj, update="irs_coverage", ix=1){
 return(get_irs_coverage(xds_obj)[ix])
}

#' Get Initial Values for Parameters
#'
#' @inheritParams get_limits_X
#'
#' @return a vector
#' @export
get_limits_X.irs_coverage <- function(xds_obj, update="irs_coverage"){
  return(c(0,1))
}
