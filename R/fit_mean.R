
#' @title Fit a seasonal pattern
#'
#' @description For a *Pf*PR time series, \eqn{x}, fit
#' a spline function
#'
#' @param pfpr_ts a PfPR time series
#' @param jdates the times of the observations
#' @param model an **`xds`** model object
#'
#' @return a list with the mean peak and the values
#' @export
fit_mean <- function(pfpr_ts, jdates, model){
  UseMethod("fit_mean", model$fitting)
}

#' @title Fit a seasonal pattern
#'
#' @description For a *Pf*PR time series, \eqn{x}, fit
#' a spline function
#'
#' @param pfpr_ts a PfPR time series
#' @param jdates the times of the observations
#' @param model an `xds` model
#'
#' @return a list with the mean peak and the values
#' @export
fit_mean.Lambda <- function(pfpr_ts, jdates, model){
  eval_mean = function(X, pfpr_ts, times, model){
    model$Lpar[[1]]$Lambda = X
    model <- xds_solve(model, times=times)
    compute_gof(model, pfpr_ts)
  }

  times <- c(model$fitting$tt[1], jdates)
  L <-  model$Lpar[[1]]$Lambda
  fitit <- stats::optimize(eval_mean, c(L/3, 3*L),
                           pfpr_ts=pfpr_ts, times=times, model=model)

  model$Lpar[[1]]$Lambda = fitit$minimum
  model <- burnin(model)
  return(model)
}
