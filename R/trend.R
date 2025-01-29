
#' @title Given data, compute GoF for a spline function
#' @description For a time series c(`times`,`data`),
#' compute the sum of squared errors for a seasonal
#' pattern defined by `Fpar`
#' @param Fpar parameters defining a spline function
#' @param data the PR observed
#' @param times the times of the observations
#' @param model the model
#' @return a list with the mean peak and the values
#' @export
sse_splinef <- function(Fpar, data, times, model){
  model$EIRpar$F_trend <- make_function(Fpar)
  model <- xds_solve_cohort(model, times=times)
  pr <- get_XH(model)$true_pr
  return(sum((data - pr)^2))
}


#' @title Fit the amplitude for a time series
#' @description For a time series \eqn{X,} compute the
#' inter-annual variability
#' @param N the number of knots
#' @param data the PR observed
#' @param times the times of the observations
#' @param model an `xds` model
#' @return a list with the mean peak and the values
#' @export
fit_splinef <- function(N, data, times, model){
  F_eval = function(X, data, times, model, Fp){
    Fp$yy = X^2
    sse_splinef(Fp, data, times, model)
  }

  knots <- seq(floor(min(times)/365)-1, ceiling(max(times)/365), by=1)*365
  yy <- 1+0*knots
  Fpar <- makepar_F_splinef(knots, yy)
  X <- stats::optim(yy, F_eval, data=data, times=times,
                                model=model, Fp=Fpar)$par

  Fpar$yy = X^2

  return(Fpar)
}
