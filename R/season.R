#' @title Compute the phase of the peak
#' @description For a time series \eqn{X,} compute the
#' phase, the time of the year when there is a peak
#' @param t the times
#' @param X the data
#' @param window days around t
#' @return a list with the mean peak and the values
#' @export
mean_phase_peak = function(t, X, window=170){
  min_t = min(t); max_t = max(t)
  peak = c()
  for(i in 1:length(t)){
    t_ix = which(t>t[i] - window & t<t[i] + window)
    ix = which.max(X[t_ix])
    peak = c(peak, t[t_ix][ix] %% 365)
  }
  return(list(mean_peak = mean(peak), full = peak))
}

#' @title Given data, compute GoF for a seasonal function
#' @description For a time series c(`times`,`data`),
#' compute the sum of squared errors for a seasonal
#' pattern defined by `Fpar`
#' @param Fpar parameters defining a seasonal pattern function
#' @param data the PR observed
#' @param times the times of the observations
#' @param model the model
#' @return a list with the mean peak and the values
#' @export
sse_season <- function(Fpar, data, times, model){
  model$EIRpar$F_season <- make_function(Fpar)
  model <- xds_solve_cohort(model, times=times)
  pr <- get_XH(model)$true_pr
  return(sum((data - pr)^2))
}

#' @title Fit the amplitude for a time series
#' @description For a time series \eqn{X,} compute the
#' phase, the time of the year when there is a peak `dog`
#' @param Fpar parameters from `makepar_F_sin`
#' @param data the PR observed
#' @param times the times of the observations
#' @param model an `xds` model
#' @return a list with the mean peak and the values
#' @export
fit_amplitude_sin_season <- function(Fpar, data, times, model){
  F_eval = function(X, data, times, model, Fp){
    Fp$floor = X[1]^2
    Fp$pw = exp(X[2])
    sse_season(Fp, data, times, model)
  }

  shift = min(times)
  times = times - shift
  Fpar$phase = Fpar$phase - shift


  pars_seas <- optim(c(sqrt(Fpar$floor), log(Fpar$pw)), F_eval,
    data = data, times = times, model = model, Fp=Fpar,
    method = "L-BFGS-B"
  )$par

  Fpar$floor = pars_seas[1]^2
  Fpar$pw = exp(pars_seas[2])
  Fpar$phase = Fpar$phase + shift

  return(Fpar)
}

#' @title Fit the phase for a time series
#' @description For a time series \eqn{X,} compute the
#' phase, the time of the year when there is a peak
#' @param data the PR observed
#' @param times the times of the observations
#' @param model an `xds` model
#' @return a list with the mean peak and the values
#' @export
fit_sin_season <- function(data, times, model){
  F_eval = function(X, data, times, model, Fp){
    Fp$floor = X[1]^2
    Fp$pw = exp(X[2])
    Fp$phase = X[3]
    sse_season(Fp, data, times, model)
  }

  Fpar <- makepar_F_sin()

  shift = min(times)
  times = times - shift
  Fpar$phase = Fpar$phase - shift


  pars_seas <- optim(c(sqrt(Fpar$floor), log(Fpar$pw), 0), F_eval,
                     data = data, times = times, model = model, Fp=Fpar,
                     method = "L-BFGS-B"
  )$par

  Fpar$floor = pars_seas[1]^2
  Fpar$pw = exp(pars_seas[2])
  Fpar$phase = Fpar$phase + shift

  return(Fpar)
}

#' @title Fit the phase for a time series
#' @description For a time series \eqn{X,} compute the
#' phase of a seasonal pattern for the EIR
#' @param data the PR observed
#' @param times the times of the observations
#' @param model an `xds` model
#' @param Fp parameters for
#' @return a list with the mean peak and the values
#' @export
fit_phase_sin_season <- function(data, times, model, Fp=NULL){
  if(is.null(Fp)) Fp <- makepar_F_sin()
  ph <- seq(0, 360, length.out=9)[-9]
  ss = ph*0
  for(i in 1:length(ph)){
    Fp$phase <- ph[i]
    ss[i] <- sse_season(Fp, data, times, model)
  }
  ix = which.min(ss)

  ph1 <- seq(ph[ix-1], ph[ix+1], length.out=9)[-9]
  ss1 = ph1*0
  for(i in 1:length(ph1)){
    Fp$phase <- ph1[i]
    ss1[i] <- sse_season(Fp, data, times, model)
  }
  ix1 <- which.min(ss1)

  ph2 <- seq(ph1[ix1-1], ph1[ix1+1], length.out=9)[-9]
  ss2 = ph2*0
  for(i in 1:length(ph2)){
    Fp$phase <- ph2[i]
    ss2[i] <- sse_season(Fp, data, times, model)
  }
  return(ph2[which.min(ss2)])
}
