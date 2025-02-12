#' @title Reconstruct a history of exposure from a PR time series
#'
#' @description Construct a function describing the EIR, including
#' the mean EIR, the seasonal pattern, and the i
#'
#' For set of paired a time series \eqn{X,} compute the
#' phase of a seasonal pattern for the EIR
#' @note This utility relies on `xds_cohort`
#' @param pr_ts the PR observed
#' @param times the times of the observations
#' @param model an `xds` model
#' @param twice cycle through a second time
#' @return an `xds` object
#' @export
pr2eir_history <- function(pr_ts, times, model, twice=TRUE){
  stopifnot(length(pr_ts) == length(times))

  # First pass: run the scaling algorithm
  mod1 <- model
  mod1$EIRpar$F_trend <- F_flat
  mod1 <- xde_scaling_eir(mod1, 25)
  t_min <- min(times)

  # Initialize the model to get the initial PR
  # to match the start of the data
  ix = which(times < t_min + 180)
  init_pr <- mean(pr_ts[ix])
  eir0 <- xde_pr2eir(init_pr, mod1, TRUE)$eir

  mod0 <- model
  mod0$EIRpar$eir <- eir0
  mod0$EIRpar$F_season <- F_flat
  mod0$EIRpar$F_trend  <- F_flat
  mod0 <- xds_solve_cohort(mod0, times = c(0, 3650))
  mod0 <- last_to_inits(mod0)
  model$Xinits[[1]] <- mod0$Xinits[[1]]


  # Set the mean EIR for the model to be the average EIR
  # from the PR time series
  mean_pr <- mean(pr_ts)
  eir1 <- xde_pr2eir(mean_pr, mod1, TRUE)$eir
  model$EIRpar$eir <- eir1

  # First pass
  model <- fit_phase_sin_season(pr_ts, times, model)
  model <- fit_amplitude_sin_season(pr_ts, times, model)
  model <- fit_spline(pr_ts, times, model)

  if(twice == TRUE){
    Ft <- model$EIRpar$F_trend
    adjust = integrate(Ft, min(times), max(times))$val/(max(times)-min(times))
    model <- set_eir(eir1/adjust, model)
    # Second pass: refit phase & amplitude
    model <- fit_phase_sin_season(pr_ts, times, model)
    model <- fit_amplitude_sin_season(pr_ts, times, model)
    model <- fit_spline(pr_ts, times, model)
  }

  tt <- seq(floor(min(times)/365), ceiling(max(times)/365), length.out=300)*365
  model <- xds_solve_cohort(model, times=tt)

  return(model)
}
