
#' Set up the xds_obj fitting object
#'
#' @param xds_obj a **`ramp.xds`**  model object
#' @param pfpr a *Pf*PR time series
#' @param jdates julian dates for `pfpr`
#' @param yr0 the starting year
#' @param t_neg_inf a date (in the past) to use for burnin
#' @param N the number of interpolation points for hindcasting and forecasting
#' @param pr_diagnostic the method used to estimate the *Pf*PR data_obj
#' @param gof_method to dispatch [compute_gof]
#'
#' @returns a **`ramp.xds`**  model object
#'
#' @export
setup_fitting = function(xds_obj, pfpr, jdates,
                         yr0=2015, t_neg_inf = 3650, N=10,
                         pr_diagnostic = "rdt",
                         gof_method = "sse"){

  xds_obj <- check_season_par(xds_obj)

  fit_obj = list()
  fit_obj$t_neg_inf <- -abs(t_neg_inf)

  class(pr_diagnostic) <- pr_diagnostic
  fit_obj$pr_diagnostic <- pr_diagnostic

  class(gof_method) <- gof_method
  fit_obj$gof <- gof_method

  xds_obj$fit_obj = fit_obj



  xds_obj <- setup_data(xds_obj, pfpr, jdates, yr0)

  xds_obj <- setup_hindcast(xds_obj, N, "use_first")

  xds_obj <- setup_forecast(xds_obj, N, "use_last")

  xds_obj <- update_fit_trend(xds_obj)

  xds_obj <- preset_phase(xds_obj)

  xds_obj <- burnin(xds_obj, t_neg_inf)

  xds_obj <- xds_solve(xds_obj, times=jdates)

  return(xds_obj)
}

#' Set up the data object for fitting
#'
#' @param xds_obj a **`ramp.xds`**  model object
#' @param pfpr a *Pf*PR time series
#' @param jdates julian dates for `pfpr`
#' @param yr0 the starting year
#'
#' @return an **`xds_obj`**
#'
#' @export
setup_data = function(xds_obj, pfpr, jdates, yr0=2015){

  yrs = floor(min(jdates/365)):ceiling(max(jdates/365))

  data_obj = list()
  data_obj$yr0 = yr0
  data_obj$years = yrs + yr0
  data_obj$ymesh = yrs*365
  data_obj$tt = yrs*365
  data_obj$yy = rep(1, length(yrs))
  data_obj$pfpr=pfpr
  data_obj$jdates=jdates

  xds_obj$data_obj <- data_obj

  xds_obj <- crude_fit_trend(xds_obj)

  return(xds_obj)
}


