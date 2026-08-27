
#' @title Build a Decomposable Time Series Model
#'
#' @description
#'
#' Like [xds_setup], [ts_setup] sets up a decomposed,
#' multiplicative time-series analysis that can be used
#' to analyze malaria *Pf*PR time series data in the
#' same way as an  **`xds`** object.
#'
#' The **`ts`** object defines `frame = class(frame) = 'ts'`
#'
#' The interface includes options to configure a function
#' describing `F_pr` as a function of time, with seasonal components
#' and a trend.
#'
#' This can be used to model a cohort as it ages;
#' a function is set up to modify exposure by age.
#'
#' @param pr is the mean pr
#' @param season_par parameters to configure a seasonality function using [ramp.func::make_function]
#' @param trend_par parameters to configure a trends function using [ramp.func::make_function]
#' @param age_par parameters to configure an age weights function using [ramp.func::make_function]
#' @param shock_par parameters to configure a shock using [ramp.func::make_function]
#' @param tnorm normalize from 0 up to tnorm
#'
#' @return a **`ts`** object
#' @export
ts_setup = function(pr=.3,
                    season_par = makepar_F_c(1),
                    trend_par = makepar_F_c(1),
                    age_par = makepar_F_c(1),
                    shock_par = makepar_F_c(1),
                    tnorm =365

){
  ts_obj <- list()
  class(ts_obj) <- "mdts"
  pr_ts <- "pr"
  class(pr_ts) = "pr"
  ts_obj$frame = pr_ts
  ts_obj$forced_by = pr_ts

  ts_obj$PR_obj <- list()
  ts_obj$PR_obj$pr <- pr
  ts_obj$PR_obj$scale <- 1
  ts_obj$PR_obj$season_par <- season_par
  ts_obj$PR_obj$trend_par <- trend_par
  ts_obj$PR_obj$age_par <- age_par
  ts_obj$PR_obj$shock_par <- shock_par
  ts_obj$PR_obj$tnorm <- tnorm
  ts_obj = rebuild_forcing_functions(ts_obj, 1)

  return(ts_obj)
}

#' Rebuild Forcing Functions
#'
#' @description
#' Rebuild forcing functions on the EIR object using `make_function`:
#' + `F_season` is made from `season_par`
#' + `F_trend` is made from `trend_par`
#' + `F_shock` is made from `shock_par`
#'
#' @inheritParams ramp.forcing::rebuild_forcing_functions
#' @keywords internal
#'
#' @return an **`xds`** object
#' @export
rebuild_forcing_functions.pr = function(xds_obj, ix=1){
  with(xds_obj$PR_obj,{
    xds_obj$PR_obj$F_season = make_function(season_par)
    xds_obj$PR_obj$F_trend = make_function(trend_par)
    xds_obj$PR_obj$F_age = make_function(age_par)
    xds_obj$PR_obj$F_shock = make_function(shock_par)
    xds_obj$PR_obj$r <- 1/200
    tm <- 1:tnorm
    fitit = function(x, obj){
      xds_obj$PR_obj$scale = x
      ts <- F_pr(tm, xds_obj)
      (mean(ts) - pr)^2
    }
    xds_obj$PR_obj$scale =  optimize(fitit, c(0, 1e3))$min
    return(xds_obj)
  })}

#' Rebuild Forcing Functions
#'
#' @description
#' Rebuild forcing functions on the EIR object using `make_function`:
#' + `F_season` is made from `season_par`
#' + `F_trend` is made from `trend_par`
#' + `F_shock` is made from `shock_par`
#'
#' @inheritParams ramp.forcing::rebuild_forcing_functions
#' @keywords internal
#'
#' @return an **`xds`** object
#' @export
F_pr = function(tm, ts_obj, bday=0){
  with(ts_obj$PR_obj,{
    age = tm-bday
    h = scale*F_season(tm)*F_trend(tm)*F_shock(tm)
    ppr = h/(h+r)
    ppr
})}

#' Set up the ts_obj fitting object
#'
#' @param ts_obj a **`ramp.xds`**  model object
#' @param pfpr a *Pf*PR time series
#' @param jdates julian dates for `pfpr`
#' @param yr0 the starting year
#' @param t_neg_inf a starting year
#' @param N the number of interpolation points for hindcasting and forecasting
#' @param gof_method to dispatch [compute_gof]
#'
#' @returns a **`ramp.xds`**  model object
#'
#' @export
setup_fitting_ts = function(ts_obj, pfpr, jdates,
                         yr0=2015, t_neg_inf = -365, N=1,
                         gof_method = "ts_sse"){

  ts_obj <- check_season_par(ts_obj)

  fit_obj = list()
  fit_obj$t_neg_inf <- -abs(t_neg_inf)

  class(pr_diagnostic) <- pr_diagnostic
  fit_obj$pr_diagnostic <- pr_diagnostic

  class(gof_method) <- gof_method
  fit_obj$gof <- gof_method

  ts_obj$fit_obj = fit_obj


  ts_obj <- setup_data(ts_obj, pfpr, jdates, yr0, N)

  ts_obj$PR_obj$pr <- mean(pfpr)

  ts_obj <- setup_hindcast(ts_obj, N, "use_first")

  ts_obj <- setup_forecast(ts_obj, N, "use_last")

  ts_obj <- update_fit_trend(ts_obj)

  return(ts_obj)
}
