

#' @title Reconstruct a history of exposure from a PR time series
#' @description Construct a function describing the EIR, including
#' the mean EIR, the seasonal pattern, and the i
#'
#'
#' For set of paired a time series \eqn{X,} compute the
#' phase of a seasonal pattern for the EIR
#' @note This utility relies on `xds_cohort`
#' @param pr_ts the PR observed
#' @param times the times of the observations
#' @param model an `xds` model
#' @return an `xds` object
#' @export
pr_ts2eir_history <- function(pr_ts, times, model){

  # First pass: set the mean eir from the mean pr
  model <- xde_scaling_eir(model, 25)
  mean_pr <- mean(pr_ts)
  model$EIRpar$eir <- xde_pr2eir(mean_pr, model, TRUE)

  # First pass: fit the phase and amplitude
  fit_phase_sin_season(pr_ts, times, model) -> phase0
  Fs0 <- make_F_sin(phase=phase0)
  Fs1 <- fit_amplitude_sin_season(Fs0, pr_ts, times, model)
  model$EIRpar$F_season <- make_function(Fs1)

  # First pass: fit the phase and amplitude
  fit_phase_sin_season(pr_ts, times, model) -> phase0
  Fs0 <- make_F_sin(phase=phase0)
  Fs1 <- fit_amplitude_sin_season(Fs0, pr_ts, times, model)
  model$EIRpar$F_season <- make_function(Fs1)



  return(model)
}
