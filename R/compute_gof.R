#' Compute GoF
#'
#' @param model a **`ramp.xds`** model object
#' @param pfpr_ts a *Pf*PR time series
#'
#' @returns sum of squared differences
#' @export
compute_gof = function(model, pfpr_ts){
  UseMethod("compute_gof", model$fitting$gof)
}

#' Compute GoF by LSS
#'
#' @param model a **`ramp.xds`** model object
#' @param pfpr_ts a *Pf*PR time series
#'
#' @returns sum of squared differences
#' @export
compute_gof.sse = function(model, pfpr_ts){
  pred_pr <- get_XH(model)$true_pr[-1]
  sum((pfpr_ts-pred_pr)^2)
}
