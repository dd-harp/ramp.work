
#' Set up the model fitting object
#'
#' @param model a **`ramp.xds`** model object
#' @param pfpr a *Pf*PR time series
#' @param jdates julian dates for `pfpr`
#' @param N the number of interpolation points for hindcasting and forecasting
#' @param forced_by emergence = Lambda; eir = eir;
#' @param pr_diagnostic the method used to estimate the *Pf*PR data
#' @param trend_par to dispatch [fit_trend]
#' @param gof to dispatch [compute_gof]
#'
#' @returns a **`ramp.xds`** model object
#' @export
setup_fitting = function(model, pfpr, jdates, N=10,
                         forced_by="Lambda",
                         pr_diagnostic = "rdt",
                         trend_par = "spline",
                         gof = "sse"){


  Sp <- ramp.xds::makepar_F_sin(bottom=2)
  model$Lpar[[1]]$season_par = Sp
  model$Lpar[[1]]$F_season <- ramp.xds::make_function(Sp)

  class(forced_by) = forced_by
  model$forced_by = forced_by

  fitting = list()
  class(fitting) <- forced_by

  class(pr_diagnostic) <- pr_diagnostic
  fitting$pr_diagnostic <- pr_diagnostic

  class(gof) <- gof
  fitting$gof <- gof

  class(trend_par) <- trend_par
  fitting$trend_par <- trend_par

  model$fitting = fitting

  data_yrs_0 = floor(min(jdates/365))
  data_yrs_n = ceiling(max(jdates/365))
  data_yrs = c(data_yrs_0:data_yrs_n)

  n_ty = length(data_yrs)
  model$fitting$n_ty = n_ty

  dta = list()
  dta$years = data_yrs
  dta$tt = data_yrs*365
  dta$yy = rep(1, n_ty)
  model$data = dta


  model <- scaling_init_ty(model, pfpr, jdates)

  model <- setup_hindcast(model, N, method = "value",
                          impute_ty = "first",
                          trust_ty = "first")

  model <- setup_forecast(model, N, method = "value",
                          impute_ty = "last",
                          trust_ty = "last")

  model <- update_F_trend(model)

  model <- burnin(model)

  return(model)
}

#' Use scaling to set initial guesses
#'
#' @description Before fitting,
#' pre-calibrate the forcing function so that it's
#' close.
#'
#' @param model a **`ramp.xds`** model object
#' @param pfpr a *Pf*PR time series
#' @param jdates julian dates for `pfpr`
#'
#' @returns a **`ramp.xds`** model object
#' @export
#'
scaling_init_ty = function(model, pfpr, jdates){
  UseMethod("scaling_init_ty", model$forced_by)
}

#' Use scaling to set initial guesses
#'
#' @description For a model forced by emergence,
#' pre-calibrate the forcing function so that it's
#' close.
#'
#' @param model a **`ramp.xds`** model object
#' @param pfpr a *Pf*PR time series
#' @param jdates julian dates for `pfpr`
#'
#' @returns a **`ramp.xds`** model object
#' @export
#'
scaling_init_ty.Lambda = function(model, pfpr, jdates){
  mean_pr <- mean(pfpr)
  L = xde_pr2Lambda(mean_pr, model)$Lambda
  model$Lpar[[1]]$Lambda = L

  yy <- model$data$yy
  tt <- model$data$tt

  for(i in 1:length(tt)){
    ix = which(abs(jdates-tt[i]) < 365)
    loc_mn = mean(pfpr[ix])
    L_loc = xde_pr2Lambda(loc_mn, model)$Lambda
    yy[i] = L_loc/L
  }

 model$data$yy <- yy

  return(model)
}

