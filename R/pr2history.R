#' @title Reconstruct a history of exposure from a PR time series
#'
#' @description Construct a function describing the EIR, including
#' the mean EIR, the seasonal pattern, and the i
#'
#' For set of paired a time series \eqn{X,} compute the
#' phase of a seasonal pattern for the EIR
#' @note This utility relies on `xds_cohort`
#' @param pfpr_ts the PR observed
#' @param jdates the jdates of the observations
#' @param model an `xds` model
#' @param twice cycle through a second time
#' @return an `xds` object
#' @export
pr2history = function(pfpr_ts, jdates, model, twice=FALSE){
  stopifnot(length(pfpr_ts) == length(jdates))

  t0 <- min(model$fitting$tt)
  times = c(t0, jdates)

  print("mean")
  model <- fit_mean(pfpr_ts, jdates, model)

  print("phase")
  model <- fit_phase(pfpr_ts, jdates, model)
  print("amplitude")
  model <- fit_amplitude(pfpr_ts, jdates, model)
  print("trend")
  model <- fit_trend_spline(pfpr_ts, jdates, model)

  if(twice == TRUE){
    print("season")
    model <- fit_season(pfpr_ts, jdates, model)
    print("trend")
    model <- fit_trend_spline(pfpr_ts, jdates, model)
  }

  return(model)
}
