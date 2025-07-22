
#' @title Fit interannual variability using splines
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
fit_trend <- function(pfpr_ts, jdates, model){
  UseMethod("fit_trend", model$fitting$trend_par)
}


#' @title Fit interannual variability using splines
#'
#' @description For a *Pf*PR time series, \eqn{x}, fit
#' a spline function
#'
#' @param pfpr_ts a PfPR time series
#' @param jdates the times of the observations
#' @param model an `xds` model
#' @param ix the interpolation points to fit
#'
#' @return a list with the mean peak and the values
#' @export
fit_trend_spline <- function(pfpr_ts, jdates, model, ix=c()){
  eval_spline = function(X, pfpr_ts, times, model, ix){
    model$fitting$data$yy[ix] <- X
    model <- update_F_trend(model)
    model <- xds_solve(model, times=times)
    compute_gof(model, pfpr_ts)
  }

  times <- c(model$fitting$tt[1], jdates)

  if(length(ix) == 0) ix = c(1:length(model$fitting$data$yy))

  inits = model$fitting$data$yy[ix]

  if(length(inits) == 1){
    fitit <- stats::optimize(eval_spline, c(0, 10), ix=ix,
                           pfpr_ts=pfpr_ts, times=times, model=model)
    X <- fitit$minimum
  } else {
    fitit <- stats::optim(inits, eval_spline, ix=ix,
                        pfpr_ts=pfpr_ts, times=times, model=model)
    X <- fitit$par
  }

  model$fitting$data$yy[ix] <- X
  model <- update_F_trend(model)
  model <- burnin(model)
  return(model)
}

#' @title Update Interpolation Points
#'
#' @description During fitting, the algorithm changes
#' one or more of the control points in
#' `model$fitting$data$yy.` The full set of control points
#' at `model$fitting$tt` and `model$fitting$yy` includes
#' some pre-observation interpolation points for burn-in, a set that
#' brackets the observation period, and post-observation
#' interpolation points to ensure continuity.
#'
#' This function copies the interpolation points from the
#' separate sets to the master set
#'
#' @param model a **`ramp.xds`** model object
#'
#' @returns a **`ramp.xds`** model object
#' @export
update_fitting_ty = function(model){
  model <- hindcast_ty(model)
  model <- forecast_ty(model)
  with(model,{
    model$fitting$tt = c(hindcast$tt, data$tt, forecast$tt)
    model$fitting$yy = c(hindcast$yy, data$yy, forecast$yy)
  return(model)
})}

#' @title Update `F_trend`
#'
#' @description Use the `fitting` interpolation points to
#' update `trend_par` and `F_trend`
#'
#' @param model a **`ramp.xds`** model object
#'
#' @returns a **`ramp.xds`** model object
#'
#' @export
update_F_trend = function(model){
  UseMethod("update_F_trend", model$fitting)
}

#' @title Update `F_trend`
#'
#' @description Use the `fitting` object to update `trend_par` and
#' `F_trend` when the model is forced by mosquito emergence, `Lambda`
#'
#' @param model a **`ramp.xds`** model object
#'
#' @returns a **`ramp.xds`** model object
#'
#' @export
update_F_trend.Lambda = function(model){
  model <- update_fitting_ty(model)
  model$Lpar[[1]]$trend_par = makepar_F_spline(model$fitting$tt, model$fitting$yy)
  model$Lpar[[1]]$F_trend = make_function(model$Lpar[[1]]$trend_par)
  return(model)
}

#' @title Update `F_trend`
#'
#' @description Use the `fitting` object to update `trend_par` and
#' `F_trend` when the model is forced by the eir
#'
#' @param model a **`ramp.xds`** model object
#'
#' @returns a **`ramp.xds`** model object
#'
#' @export
update_F_trend.eir = function(model){
  model <- update_fitting_ty(model)
  model$EIRpar$trend_par = makepar_F_spline(model$fitting$tt, model$fitting$yy)
  model$EIRpar$F_trend = make_function(model$EIRpar[[1]]$trend_par)
  return(model)
}


#' @title Update `F_trend`
#'
#' @description Use the `fitting` object to update `trend_par` and
#' `F_trend` when the model is forced by the eir
#'
#' @param yy the \eqn{y} value for interpolation
#' @param ix the index of interpolation point to replace
#' @param model a **`ramp.xds`** model object
#'
#' @returns a **`ramp.xds`** model object
#'
#' @export
change_yy = function(yy, ix, model){
  model$fitting$data$yy[ix] = yy
  model <- update_F_trend(model)
  return(model)
}

