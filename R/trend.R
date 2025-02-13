
#' @title Given data, compute GoF for a spline function
#' @description For a time series c(`times`,`data`),
#' compute the sum of squared errors for a seasonal
#' pattern defined by `Fpar`
#' @param data the PR observed
#' @param times the times of the observations
#' @param model the model
#' @return a list with the mean peak and the values
#' @export
sse_spline <- function(data, times, model){
  model <- xds_solve_cohort(model, times=times)
  pr <- get_XH(model)$true_pr[-1]
  return(sum((data - pr)^2))
}


#' @title Fit the amplitude for a time series
#' @description For a time series \eqn{X,} compute the
#' inter-annual variability
#' @param data the PR observed
#' @param times the times of the observations
#' @param model an `xds` model
#' @param splinef which type of spline
#' @return a list with the mean peak and the values
#' @export
fit_spline <- function(data, times, model, splinef=2){
  F_eval = function(X, data, times, model){
 #   last = tail(X)[1]
    model$EIRpar$trend_par$yy = c(X[1], X)
    sse_spline(data, times, model)
  }

  stopifnot(length(model$EIRpar$season_par)>0)
  model$EIRpar$season_par$phase -> phase

  times <- c(-365, times)
  knots <- seq(floor(min(times)/365), ceiling(max(times)/365), by=1)*365 + phase
  yy <- 1+0*knots
  par0 <- makepar_F_spline(knots, yy, X=2)
  model$EIRpar$trend_par <- par0
  ll = length(yy)-1

  X <- stats::optim(rep(1, ll), F_eval, data=data, times=times,
                                model=model)$par
  model$EIRpar$trend_par$yy = c(X[1], X)
  model <- xds_solve_cohort(model, times=times)
  return(model)
}
