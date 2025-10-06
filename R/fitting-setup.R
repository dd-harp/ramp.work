
#' Set up the xds_obj fitting object
#'
#' @param xds_obj a **`ramp.xds`**  model object
#'
#' @param pfpr a *Pf*PR time series
#' @param jdates julian dates for `pfpr`
#'
#' @param t_neg_inf a date (in the past) to use for burnin
#' @param N the number of interpolation points for hindcasting and forecasting
#' @param pr_diagnostic the method used to estimate the *Pf*PR data
#' @param gof_method to dispatch [compute_gof]
#'
#' @returns a **`ramp.xds`**  model object
#'
#' @export
setup_fitting = function(xds_obj, pfpr, jdates,
                         t_neg_inf = 3650,
                         N=10,
                         pr_diagnostic = "rdt",
                         gof_method = "sse"){

  xds_obj <- check_season_par(xds_obj)

  fitting = list()

  class(pr_diagnostic) <- pr_diagnostic
  fitting$pr_diagnostic <- pr_diagnostic

  class(gof_method) <- gof_method
  fitting$gof <- gof_method

  fitting$t_neg_inf <- -abs(t_neg_inf)

  xds_obj$fitting = fitting

  # The data object
  data <- list()
  data$pfpr = pfpr
  data$jdates = jdates
  xds_obj$data = setup_spline_fit(data)

  # The data object
  xds_obj <- scaling_init_ty(xds_obj)

  xds_obj <- setup_hindcast(xds_obj, N,
                          method = "use_first")

  xds_obj <- setup_forecast(xds_obj, N,
                          method = "use_last")

  xds_obj <- update_fitting_ty(xds_obj)

  xds_obj <- setup_trend_par(xds_obj)

  xds_obj <- preset_phase(xds_obj)

  xds_obj <- burnin(xds_obj, t_neg_inf)

  xds_obj <- xds_solve(xds_obj, times=jdates)

  return(xds_obj)
}

#' Title
#'
#' @param data a list with
#'
#' @return a data object for fitting
#' @export
setup_spline_fit = function(data){
  with(data,{

    data_yrs_0 = floor(min(jdates/365))
    data_yrs_n = ceiling(max(jdates/365))
    data_yrs = c(data_yrs_0:data_yrs_n)

    n_ty = length(data_yrs)

    data$n_ty = n_ty
    data$years = data_yrs
    data$tt = data_yrs*365
    data$yy = rep(1, n_ty)
    return(data)
})}

#' Check the trend setup
#'
#' @param xds_obj a **`ramp.xds`** model object
#'
#' @return a **`ramp.xds`** model object
#' @export
setup_trend_par = function(xds_obj){
  UseMethod("setup_trend_par", xds_obj$forced_by)
}

#' Check the trend setup
#'
#' @param xds_obj a **`ramp.xds`** model object
#'
#' @return a **`ramp.xds`** model object
#' @export
setup_trend_par.Lambda= function(xds_obj){
  Tp <- with(xds_obj$fitting, makepar_F_spline(tt, yy, X=2))
  xds_obj$L_obj[[1]]$trend_par <- Tp
  xds_obj$L_obj[[1]]$F_trend <- make_function(Tp)
  return(xds_obj)
}

#' Check the trend setup
#'
#' @param xds_obj a **`ramp.xds`** model object
#'
#' @return a **`ramp.xds`** model object
#' @export
setup_trend_par.eir = function(xds_obj){
  Tp <- with(xds_obj$fitting, makepar_F_spline(tt, yy, X=2))
  xds_obj$EIR_obj$trend_par <- Tp
  xds_obj$EIR_obj$F_trend <- make_function(Tp)
  return(xds_obj)
}

#' Check the seasonal setup
#'
#' @param xds_obj a **`ramp.xds`** model object
#'
#' @return a **`ramp.xds`** model object
#' @export
check_season_par = function(xds_obj){
  UseMethod("check_season_par", xds_obj$forced_by)
}

#' Check the seasonal setup
#'
#' @param xds_obj a **`ramp.xds`** model object
#'
#' @return a **`ramp.xds`** model object
#' @export
check_season_par.Lambda= function(xds_obj){
  Sp <- setup_season(xds_obj$L_obj[[1]]$season_par)
  xds_obj$L_obj[[1]]$season_par <- Sp
  xds_obj$L_obj[[1]]$F_season <- make_function(Sp)
  return(xds_obj)
}

#' Check the seasonal setup
#'
#' @param xds_obj a **`ramp.xds`** model object
#'
#' @return a **`ramp.xds`** model object
#' @export
check_season_par.eir = function(xds_obj){
  Sp <- setup_season(xds_obj$EIR_obj$season_par)
  xds_obj$EIR_obj$season_par <- Sp
  xds_obj$EIR_obj$F_season <- make_function(Sp)
  return(xds_obj)
}

#' Set up the seasonal pattern
#'
#' @param season_par seasonal pattern parameters
#'
#' @return a `sin` function parameter set
#' @export
#' @seealso [makepar_F_sin()]
setup_season = function(season_par){
  UseMethod("setup_season", season_par)
}

#' Set up the seasonal pattern
#'
#' @description
#' If the current `season_par` is a list,
#' return a parameter set
#'
#' @param season_par seasonal pattern parameters
#'
#' @return a `sin` function parameter set
#'
#' @seealso [makepar_F_sin()]
#'
#' @export
#'
setup_season.list = function(season_par){
  makepar_F_sin(bottom=2)
}

#' Set up the seasonal pattern
#'
#' @description
#' If the current `season_par` is from the `sin`
#' family of functions, return the parameters unchanged
#'
#' @param season_par seasonal pattern parameters
#'
#' @return a `sin` function parameter set
#'
#' @seealso [makepar_F_sin()]
#'
#' @export
#'
setup_season.sin = function(season_par){
  season_par
}

#' Use scaling to set initial guesses
#'
#' @description Before fitting,
#' pre-calibrate the forcing function so that it's
#' close.
#'
#' @param xds_obj a **`ramp.xds`**  model object
#'
#' @returns a **`ramp.xds`**  model object
#' @export
#'
scaling_init_ty = function(xds_obj){
  UseMethod("scaling_init_ty", xds_obj$forced_by)
}

#' Use scaling to set initial guesses
#'
#' @description For a xds_obj forced by emergence,
#' pre-calibrate the forcing function so that it's
#' close.
#'
#' @param xds_obj a **`ramp.xds`**  model object
#'
#' @returns a **`ramp.xds`**  model object
#'
#' @export
scaling_init_ty.Lambda = function(xds_obj){

  mean_pr <- mean(xds_obj$data$pfpr)
  xds_pr2Lambda(mean_pr, xds_obj)$Lambda -> L
  xds_obj$L_obj[[1]]$Lambda <- L

#  xds_obj <- fit_mean_forcing(xds_obj)
#  L <- xds_obj$L_obj[[1]]$Lambda

  yy <- xds_obj$data$yy
  tt <- xds_obj$data$tt

  for(i in 1:length(tt)){
    ix = which(abs(xds_obj$data$jdates-tt[i]) < 365)
    loc_mn = mean(xds_obj$data$pfpr[ix])
    L_loc = xds_pr2Lambda(loc_mn, xds_obj)$Lambda
    yy[i] = L_loc/L
  }

  xds_obj$data$yy <- yy

  return(xds_obj)
}

#' Use scaling to set initial guesses
#'
#' @description For a xds_obj forced by emergence,
#' pre-calibrate the forcing function so that it's
#' close.
#'
#' @param xds_obj a **`ramp.xds`**  model object
#'
#' @returns a **`ramp.xds`**  model object
#'
#' @export
scaling_init_ty.eir = function(xds_obj){

  mean_pr <- mean(xds_obj$data$pfpr)
  xds_pr2eir(mean_pr, xds_obj)$eir -> E
  xds_obj$EIR_obj$eir <- E

  yy <- xds_obj$data$yy
  tt <- xds_obj$data$tt

  for(i in 1:length(tt)){
    ix = which(abs(xds_obj$data$jdates-tt[i]) < 365)
    loc_mn = mean(xds_obj$data$pfpr[ix])
    E_loc = xds_pr2eir(loc_mn, xds_obj)$eir
    yy[i] = E_loc/E
  }

  xds_obj$data$yy <- yy

  return(xds_obj)
}
