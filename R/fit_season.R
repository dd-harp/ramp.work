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
fit_season <- function(pfpr_ts, jdates, model){

  eval_season_fit = function(X, pfpr_ts, times, model){
    model <- change_F_season(X, model)
    model <- xds_solve(model, times=times)
    compute_gof(model, pfpr_ts)
  }

  times <- c(model$fitting$tt[1], jdates)
  inits = with(model$Lpar[[1]]$season_par, c(phase, pw, bottom))
  fitit <- stats::optim(inits, eval_season_fit,
                        pfpr_ts=pfpr_ts, times=times, model=model)
  X <- fitit$par
  model <- change_F_season(X, model)
  model <- burnin(model)
  return(model)
}

#' Change F_season
#'
#' @param X the value of the parameters
#' @param model a **`ramp.xds`** model object
#'
#' @returns a **`ramp.xds`** model object
#' @export
change_F_season = function(X, model){
  UseMethod("change_F_season", model$fitting)
}

#' Change F_season
#'
#' @inheritParams change_F_season
#'
#' @returns a **`ramp.xds`** model object
#' @export
change_F_season.Lambda = function(X, model){
  model$Lpar[[1]]$season_par$phase = X[1]
#  Fpw = function(x){1 + 8*x^2/(1+x^2)}
  Fpw = function(x){x}
  model$Lpar[[1]]$season_par$pw = Fpw(X[2])
  model$Lpar[[1]]$season_par$bottom = X[3]
  model$Lpar[[1]]$F_season <- make_function(model$Lpar[[1]]$season_par)
  return(model)
}

#' @title Fit the phase
#'
#' @description For a *Pf*PR time series, \eqn{x}, fit
#' the phase for a seasonal pattern. The algorithm works
#' by choosing an initial mesh, around 46 days, and then
#' shrinking the mesh around the value that maximizes
#' the goodness of fit. The \eqn{n} parameter determines
#' how many times the process is iterated. For \eqn{n=2},
#' the mesh is
#'
#' @param pfpr_ts a PfPR time series
#' @param jdates the times of the observations
#' @param model an `xds` model
#' @param n number of times to iterate
#'
#' @return a list with the mean peak and the values
#' @export
fit_phase <- function(pfpr_ts, jdates, model, n=2){
  eval_phase = function(phase, pfpr_ts, times, model){
    model = change_F_season_phase(phase, model)
    model = xds_solve(model, times=times)
    compute_gof(model, pfpr_ts)
  }

  times <- c(model$fitting$tt[1], jdates)

  phase <- seq(0, 365, length.out=9)[-1]
  gof = phase*0
  for(i in 1:8) gof[i] = eval_phase(phase[i], pfpr_ts, times, model)

  ix = which.min(gof)
  ff <- gof[ix]
  best <- phase[ix]

  for(iter in c(1:n)){
    df = max(diff(phase))
    phase <- seq(best-df, best+df, length.out=9)[-1]
    gof = phase*0
    for(i in 1:8){
      gof[i] = eval_phase(phase[i], pfpr_ts, times, model)
    }
    ix = which.min(gof)
    ff <- gof[ix]
    best <- phase[ix]
  }

  model <- change_F_season_phase(best, model)
  model <- burnin(model)
  return(model)
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
fit_phase_alt <- function(pfpr_ts, jdates, model){
  eval_phase = function(X, pfpr_ts, times, model){
    model <- change_F_season_phase(X, model)
    model <- xds_solve(model, times=times)
    compute_gof(model, pfpr_ts)
  }

  times <- c(model$fitting$tt[1], jdates)

  fitit <- stats::optimize(eval_phase, c(0, 365),
                           pfpr_ts=pfpr_ts, times=times, model=model)

  model <- change_F_season_phase(fitit$minimum, model)
  model <- burnin(model)
  return(model)
}

#' Change F_season
#'
#' @param phase the value of the parameters
#' @param model a **`ramp.xds`** model object
#'
#' @returns a **`ramp.xds`** model object
#' @export
change_F_season_phase = function(phase, model){
  UseMethod("change_F_season_phase", model$fitting)
}

#' Change F_season
#'
#' @inheritParams change_F_season_phase
#'
#' @returns a **`ramp.xds`** model object
#' @export
change_F_season_phase.Lambda = function(phase, model){
  model$Lpar[[1]]$season_par$phase = phase
  model$Lpar[[1]]$F_season <- make_function(model$Lpar[[1]]$season_par)
  return(model)
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
fit_amplitude <- function(pfpr_ts, jdates, model){
  eval_amplitude = function(X, pfpr_ts, times, model){
    model <- change_F_season_amplitude(X, model)
    model <- xds_solve(model, times=times)
    compute_gof(model, pfpr_ts)
  }

  times <- c(model$fitting$tt[1], jdates)

  fitit <- stats::optimize(eval_amplitude, c(0, 10),
                           pfpr_ts=pfpr_ts, times=times, model=model)

  model <- change_F_season_amplitude(fitit$minimum, model)
  model <- burnin(model)
  return(model)
}

#' Change F_season
#'
#' @param X the value of the parameters
#' @param model a **`ramp.xds`** model object
#'
#' @returns a **`ramp.xds`** model object
#' @export
change_F_season_amplitude = function(X, model){
  UseMethod("change_F_season_amplitude", model$fitting)
}

#' Change F_season
#'
#' @inheritParams change_F_season_amplitude
#'
#' @returns a **`ramp.xds`** model object
#' @export
change_F_season_amplitude.Lambda = function(X, model){
  model$Lpar[[1]]$season_par$bottom = X
  model$Lpar[[1]]$F_season <- make_function(model$Lpar[[1]]$season_par)
  return(model)
}




#' Change F_season
#'
#' @inheritParams change_F_season
#'
#' @returns a **`ramp.xds`** model object
#' @export
change_F_season.eir= function(X, model){
  model$EIRpar$season_par$phase = X[1]
  model$EIRpar$season_par$amplitude = X[2]
  model$EIRpar$season_par$amplitude = X[2]
  model$EIRpar$F_season <- make_function(model$EIRpar$season_par)
}
