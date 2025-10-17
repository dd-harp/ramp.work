
#' Compute GoF
#'
#' @param xds_obj a **`ramp.xds`** model object
#'
#' @returns sum of squared differences
#' @export
compute_gof = function(xds_obj){
  UseMethod("compute_gof", xds_obj$fit_obj$gof)
}

#' Compute SSE
#'
#' @description
#' Computes the sum of squared errors
#'
#'
#' @param xds_obj a **`ramp.xds`** model object
#'
#' @returns sum of squared differences
#' @export
compute_gof.sse = function(xds_obj){
  times = c(xds_obj$fit_obj$t_neg_inf, xds_obj$data_obj$jdates)
  xds_obj <- xds_solve(xds_obj, times=times)
  pred_pr <- get_PR(xds_obj, i=1, method=xds_obj$fit_obj$pr_diagnostic)[-1]
  sum((xds_obj$data_obj$pfpr-pred_pr)^2)
}


