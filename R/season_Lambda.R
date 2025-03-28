
#' @title Given data, compute GoF for a seasonal function
#' @description For a time series c(`times`,`data`),
#' and a model, compute the sum of squared errors
#' @param data the PR observed
#' @param times the times of the observations
#' @param model the model
#' @return a list with the mean peak and the values
#' @export
sse_Lambda_season <- function(data, times, model){
  model$Lpar[[1]]$F_season <- make_function(model$Lpar[[1]]$season_par)
  model <- xds_solve(model, times=times)
  pr <- get_XH(model)$true_pr[-1]
  return(sum((data - pr)^2))
}


#' @title Fit the phase for a time series
#' @description For a time series \eqn{X,} compute the
#' phase of a seasonal pattern for the Lambda
#' @param data the PR observed
#' @param times the times of the observations
#' @param model an `xds` object
#' @param Fp initial parameters for seasonality
#' @return an `xds` object
#' @export
fit_Lambda_phase_sin_season <- function(data, times, model, Fp=NULL){

  # make sure that season_par exists
  if(!is.null(Fp))
    model$Lambdapar$season_par <- Fp
  if(length(model$Lambdapar$season_par)==0)
    model$Lambdapar$season_par <- makepar_F_sin()

  times = c(-365, times)
  # 45 days
  ph <- seq(0, 360, by = 45)
  ss = ph*0
  for(i in 1:length(ph)){
    model$Lpar[[1]]$season_par$phase <- ph[i]
    ss[i] <- sse_Lambda_season(data, times, model)
  }
  ix = which.min(ss)
  ff <- ss[ix]
  best <- ph[ix]

  # 10 days apart
  ph1 <- seq(ph[ix]-40, ph[ix]+40, by=10)
  ss1 = ph1*0
  for(i in 1:length(ph1)){
    model$Lpar[[1]]$season_par$phase <- ph1[i]
    ss1[i] <- sse_Lambda_season(data, times, model)
  }
  ix1 <- which.min(ss1)
  ff1 <- ss1[ix1]
  best1 <- ph1[ix1]

  # 1 day apart
  if(ff1<ff) best=best1
  ph2 <- seq(best-4, best+4, by=1)
  ss2 = ph2*0
  for(i in 1:length(ph2)){
    model$Lpar[[1]]$season_par$phase <- ph2[i]
    ss2[i] <- sse_Lambda_season(data, times, model)
  }

  model$Lpar[[1]]$season_par$phase = ph2[which.min(ss2)]
  model$Lpar[[1]]$F_season <- make_function(model$Lpar[[1]]$season_par)

  model <- xds_solve(model, times=times)
  return(model)
}

#' @title Fit the amplitude for a time series
#' @description For a time series \eqn{X,} compute the
#' phase, the time of the year when there is a peak `dog`
#' @param data the PR observed
#' @param times the times of the observations
#' @param model an `xds` model
#' @param Fpar parameters from `makepar_F_sin`
#' @return an `xds` object
#' @export
fit_Lambda_amplitude_sin_season <- function(data, times, model, Fpar=NULL){

  Fpw = function(x){1 + 8*x^2/(1+x^2)}

  F_eval = function(X, data, times, model){
    model$Lpar[[1]]$season_par$bottom = X[1]^2
    model$Lpar[[1]]$season_par$pw = Fpw(X[2])
    sse_Lambda_season(data, times, model)
  }

  # make sure that season_par exists
  if(!is.null(Fpar))
    model$Lpar[[1]]$season_par <- Fpar

  if(length(model$Lpar[[1]]$season_par)==0)
    model$Lpar[[1]]$season_par <- makepar_F_sin()

  times <- c(-365, times)
  model <- xds_solve(model, times=times)
  pr <- get_XH(model)$true_pr
  stopifnot(length(pr)-1 == length(data))

  pars_seas <- stats::optim(c(1, 1), F_eval,
                            data=data, times=times, model=model, method="L-BFGS-B")$par

  model$Lpar[[1]]$season_par$bottom = pars_seas[1]^2
  model$Lpar[[1]]$season_par$pw = Fpw(pars_seas[2])
  model$Lpar[[1]]$F_season <- make_function(model$Lpar[[1]]$season_par)
  model <- xds_solve(model, times=times)

  return(model)
}

#' @title Fit the phase for a time series
#' @description For a time series \eqn{X,} compute the
#' phase, the time of the year when there is a peak
#' @param data the PR observed
#' @param times the times of the observations
#' @param model an `xds` object
#' @param Fpar parameters from `makepar_F_sin`
#' @return an `xds` object
#' @export
fit_Lambda_sin_season <- function(data, times, model, Fpar=NULL){

  Fpw = function(x){1 + 8*x^2/(1+x^2)}

  F_eval = function(X, data, times, model){
    model$Lpar[[1]]$season_par$bottom = X[1]^2
    model$Lpar[[1]]$season_par$pw = Fpw(X[2])
    model$Lpar[[1]]$season_par$phase = X[3]
    sse_Lambda_season(data, times, model)
  }

  if(length(model$Lpar[[1]]$season_par)==0)
    model$Lpar[[1]]$season_par <- makepar_F_sin()

  pars_seas <- stats::optim(c(sqrt(Fpar$bottom), log(Fpar$pw), 0), F_eval,
                            data = data, times = times, model = model)$par

  model$Lpar[[1]]$season_par$bottom = pars_seas[1]^2
  model$Lpar[[1]]$season_par$pw = Fpw(pars_seas[2])
  model$Lpar[[1]]$season_par$phase = pars_seas[3]
  model$Lpar[[1]]$F_season <- make_function(model$Lpar[[1]]$season_par)
  model <- xds_solve_cohort(model, times=times)
  return(model)
}
