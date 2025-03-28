
#' @title Given data, compute GoF for a spline function
#' @description For a time series c(`times`,`data`),
#' compute the sum of squared errors for a seasonal
#' pattern defined by `Fpar`
#' @param data the PR observed
#' @param times the times of the observations
#' @param model the model
#' @return a list with the mean peak and the values
#' @export
sse_EIR_spline <- function(data, times, model){
  model <- xds_solve_cohort(model, times=times)
  pr <- get_XH(model)$true_pr[-1]
  return(sum((data - pr)^2))
}

#' @title Given data, compute GoF for a spline function
#' @description For a time series c(`times`,`data`),
#' compute the sum of squared errors for a seasonal
#' pattern defined by `Fpar`
#' @param data the PR observed
#' @param times the times of the observations
#' @param model the model
#' @return a list with the mean peak and the values
#' @export
sse_Lambda_spline <- function(data, times, model){
  model <- xds_solve(model, times=times)
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
fit_EIR_spline <- function(data, times, model, splinef=2){
  F_eval = function(X, data, times, model){
 #   last = tail(X)[1]
    model$EIRpar$trend_par$yy = c(X[1], X)
    sse_EIR_spline(data, times, model)
  }
  times <- c(-365, times)
  phase <- model$Lpar[[1]]$season_par$phase
  if(is.null(phase)) phase = 0
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

#' @title Fit the amplitude for a time series
#' @description For a time series \eqn{X,} compute the
#' inter-annual variability
#' @param data the PR observed
#' @param times the times of the observations
#' @param model an `xds` model
#' @param splinef which type of spline
#' @return a list with the mean peak and the values
#' @export
fit_Lambda_spline <- function(data, times, model, splinef=2){
   F_eval = function(X, data, times, model){
 #   last = tail(X)[1]
    model$Lpar[[1]]$trend_par$yy = c(X[1], X)
    model$Lpar[[1]]$F_trend <- make_function(model$Lpar[[1]]$trend_par)
    sse_Lambda_spline(data, times, model)
  }
  times <- c(-365, times)
  phase <- model$Lpar[[1]]$season_par$phase
  knots <- seq(floor(min(times)/365), ceiling(max(times)/365), by=1)*365 + phase
  yy <- 1+0*knots
  par0 <- makepar_F_spline(knots, yy, X=2)
  model$Lpar[[1]]$trend_par <- par0
  model$Lpar[[1]]$F_trend <- make_function(model$Lpar[[1]]$trend_par)
  ll = length(yy)-1

  X <- stats::optim(rep(1, ll), F_eval, data=data, times=times,
                                model=model)$par
  model$Lpar[[1]]$trend_par$yy = c(X[1], X)
  model$Lpar[[1]]$F_trend <- make_function(model$Lpar[[1]]$trend_par)
  model <- xds_solve(model, times=times)
  return(model)
}
