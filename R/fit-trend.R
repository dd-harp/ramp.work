

#' @title feature Interpolation Points
#'
#' @description
#' This function copies the interpolation points from the
#' `hindcast` and `data_obj` and `forecast` to `fit_obj.`
#'
#' During fit_obj, the algorithm changes
#' one or more of the \eqn{y} values of the
#' interpolation points in
#' `xds_obj$data_obj$yy.` The xds_obj then calls
#' `hindcast_ty` and `forecast_ty` to feature `xds_obj$hindcast$y` (the
#' pre-observation interpolation points for burn-in) and `xds_obj$hindcast$y`
#' (post-observation interpolation points).
#' These are copied to the
#' full set of control points stored at
#' at `xds_obj$fit_obj$tt` and `xds_obj$fit_obj$yy.`
#'
#' The rules for `hindcast_ty`  and `forecast_ty`
#' are setup
#' by `setup_hindcast` and `setup_forecast.`
#'
#'
#' @param xds_obj a **`ramp.xds`** xds_obj object
#'
#' @returns a **`ramp.xds`** xds_obj object
#' @export
update_fit_trend = function(xds_obj){
  xds_obj <- hindcast_ty(xds_obj)
  xds_obj <- forecast_ty(xds_obj)
  with(xds_obj,{
    xds_obj$fit_obj$tt = c(hindcast$tt, data_obj$tt, forecast$tt)
    xds_obj$fit_obj$yy = c(hindcast$yy, data_obj$yy, forecast$yy)
    xds_obj <- compile_fit_trend(xds_obj)
    return(xds_obj)
})}

#' Check the trend setup
#'
#' @param xds_obj a **`ramp.xds`** model object
#'
#' @return a **`ramp.xds`** model object
#' @export
compile_fit_trend = function(xds_obj){
  UseMethod("compile_fit_trend", xds_obj$forced_by)
}

#' Check the trend setup
#'
#' @param xds_obj a **`ramp.xds`** model object
#'
#' @return a **`ramp.xds`** model object
#' @export
compile_fit_trend.Lambda= function(xds_obj){
  Tp <- with(xds_obj$fit_obj, makepar_F_spline(tt, yy, X=2))
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
compile_fit_trend.eir = function(xds_obj){
  Tp <- with(xds_obj$fit_obj, makepar_F_spline(tt, yy, X=2))
  xds_obj$EIR_obj$trend_par <- Tp
  xds_obj$EIR_obj$F_trend <- make_function(Tp)
  return(xds_obj)
}

#' Get Spline \eqn{t, y} Values
#'
#' @param xds_obj a **`ramp.xds`**  model object
#'
#' @returns a **`ramp.xds`**  model object
#'
#' @export
get_fit_trend = function(xds_obj){
  yy <- xds_obj$data_obj$yy
  tt <- xds_obj$data_obj$tt
  return(list(tt=tt, yy=yy))
}

#' Replace Spline \eqn{y} Values
#'
#' @param yy a new set of \eqn{y} values for the interpolation points
#' @param tt a new set of \eqn{t} values for the interpolation points
#' @param xds_obj a **`ramp.xds`**  model object
#'
#' @returns a **`ramp.xds`**  model object
#'
#' @export
change_all_fit_spline_ty = function(yy, tt, xds_obj){
  stopifnot(length(yy) == length(tt))
  xds_obj$data_obj$yy = yy
  xds_obj$data_obj$tt = tt
  xds_obj$data_obj$n_ty = length(yy)
  xds_obj <- update_fit_trend(xds_obj)
  return(xds_obj)
}


#' Replace Spline \eqn{t} Values
#'
#' @param new_t a new set of \eqn{t} values for the interpolation points
#' @param xds_obj a **`ramp.xds`**  model object
#'
#' @returns a **`ramp.xds`**  model object
#'
#' @export
change_all_fit_spline_t = function(new_t, xds_obj){
  xds_obj$data_obj$tt = new_t
  xds_obj$data_obj$yy = F_trend(new_t, xds_obj)
  xds_obj$data_obj$n_ty = length(new_t)
  xds_obj <- update_fit_trend(xds_obj)
  return(xds_obj)
}

#' Replace Spline \eqn{y} Values
#'
#' @param new_y a new set of \eqn{y} values for the interpolation points
#' @param xds_obj a **`ramp.xds`**  model object
#'
#' @returns a **`ramp.xds`**  model object
#'
#' @export
change_all_fit_spline_y = function(new_y, xds_obj){
  stopifnot(length(new_y) == length(xds_obj$data_obj$tt))
  xds_obj$data_obj$yy = new_y
  xds_obj <- update_fit_trend(xds_obj)
  return(xds_obj)
}

#' Change Spline \eqn{t} Values
#'
#' @param new_t the new \eqn{t} interpolation point value(s)
#' @param t_ix the indices of the values to replace
#' @param xds_obj a **`ramp.xds`**  model object
#'
#' @returns a **`ramp.xds`**  model object
#'
#' @export
change_ix_fit_spline_t = function(new_t, t_ix, xds_obj){
  stopifnot(length(new_t) == length(t_ix))
  xds_obj$data_obj$tt[t_ix] = new_t
  xds_obj$data_obj$yy = F_trend(new_t, xds_obj)
  xds_obj <- update_fit_trend(xds_obj)
  return(xds_obj)
}

#' Change Spline \eqn{y} Values
#'
#' @param new_y the new \eqn{y} interpolation point value(s)
#' @param y_ix the indices of the values to replace
#' @param xds_obj a **`ramp.xds`**  model object
#'
#' @returns a **`ramp.xds`**  model object
#'
#' @export
change_ix_fit_spline_y = function(new_y, y_ix, xds_obj){
  stopifnot(length(new_y) == length(y_ix))
  xds_obj$data_obj$yy[y_ix] = new_y
  xds_obj$data_obj$yy[y_ix] = new_y
  xds_obj <- update_fit_trend(xds_obj)
  return(xds_obj)
}

#' Change Spline \eqn{t,y} Values
#'
#' @param new_y the new \eqn{y} interpolation point value(s)
#' @param new_t the new \eqn{t} interpolation point value(s)
#' @param ix the indices of the values to replace
#' @param xds_obj a **`ramp.xds`**  model object
#'
#' @returns a **`ramp.xds`**  model object
#'
#' @export
change_ix_fit_spline_ty = function(new_y, new_t, ix, xds_obj){
  stopifnot(length(new_y) == length(ix))
  stopifnot(length(new_t) == length(ix))
  xds_obj$data_obj$yy[ix] = new_y
  xds_obj$data_obj$tt[ix] = new_t
  xds_obj <- update_fit_trend(xds_obj)
  return(xds_obj)
}

#' Change Spline \eqn{t,y} Values
#'
#' @param new_y the new \eqn{y} interpolation point value(s)
#' @param new_t the new \eqn{t} interpolation point value(s)
#' @param xds_obj a **`ramp.xds`**  model object
#'
#' @returns a **`ramp.xds`**  model object
#'
#' @export
add_fit_spline_ty = function(new_y, new_t, xds_obj){
  stopifnot(length(new_y) == length(new_t))
  xds_obj$data_obj$yy = c(xds_obj$data_obj$yy, new_y)
  xds_obj$data_obj$tt = c(xds_obj$data_obj$tt, new_t)
  ot = order(xds_obj$data_obj$tt)
  xds_obj$data_obj$tt = xds_obj$data_obj$tt[ot]
  xds_obj$data_obj$yy = xds_obj$data_obj$yy[ot]
  xds_obj <- update_fit_trend(xds_obj)
  return(xds_obj)
}


#' Change Spline \eqn{t,y} Values
#'
#' @param ix the indices of the values to replace
#' @param xds_obj a **`ramp.xds`**  model object
#'
#' @returns a **`ramp.xds`**  model object
#'
#' @export
rm_ix_fit_spline_ty = function(ix, xds_obj){
  xds_obj$data_obj$yy = xds_obj$data_obj$yy[-ix]
  xds_obj$data_obj$tt = xds_obj$data_obj$tt[-ix]
  xds_obj$data_obj$n_ty = length(xds_obj$data_obj$yy)
  xds_obj <- update_fit_trend(xds_obj)
  return(xds_obj)
}


#' Initialize the trend
#'
#' @description Before fitting,
#' pre-calibrate the forcing function so that it's
#' "close."
#'
#' @param xds_obj a **`ramp.xds`**  model object
#'
#' @returns a **`ramp.xds`**  model object
#' @export
#'
crude_fit_trend = function(xds_obj){
  UseMethod("crude_fit_trend", xds_obj$forced_by)
}

#' Initialize the trend
#'
#' @description For a model forced by emergence,
#' pre-calibrate the forcing function so that it's
#' close.
#'
#' @param xds_obj a **`ramp.xds`**  model object
#'
#' @returns a **`ramp.xds`**  model object
#'
#' @export
crude_fit_trend.Lambda = function(xds_obj){

  mean_pr <- mean(xds_obj$data_obj$pfpr)
  xds_pr2Lambda(mean_pr, xds_obj)$Lambda -> L
  xds_obj$L_obj[[1]]$Lambda <- L

  #  xds_obj <- fit_mean_forcing(xds_obj)
  #  L <- xds_obj$L_obj[[1]]$Lambda

  yy <- xds_obj$data_obj$yy
  tt <- xds_obj$data_obj$tt

  for(i in 1:length(tt)){
    ix = which(abs(xds_obj$data_obj$jdates-tt[i]) < 365)
    loc_mn = mean(xds_obj$data_obj$pfpr[ix])
    L_loc = xds_pr2Lambda(loc_mn, xds_obj)$Lambda
    yy[i] = L_loc/L
  }

  xds_obj$data_obj$yy <- yy

  return(xds_obj)
}

#' Initialize the trend
#'
#' @description For a model forced by an EIR trace function,
#' pre-calibrate the forcing function so that it's close.
#'
#' @param xds_obj a **`ramp.xds`**  model object
#'
#' @returns a **`ramp.xds`**  model object
#'
#' @export
crude_fit_trend.eir = function(xds_obj){

  mean_pr <- mean(xds_obj$data_obj$pfpr)
  xds_pr2eir(mean_pr, xds_obj)$eir -> E
  xds_obj$EIR_obj$eir <- E

  yy <- xds_obj$data_obj$yy
  tt <- xds_obj$data_obj$tt

  for(i in 1:length(tt)){
    ix = which(abs(xds_obj$data_obj$jdates-tt[i]) < 365)
    loc_mn = mean(xds_obj$data_obj$pfpr[ix])
    E_loc = xds_pr2eir(loc_mn, xds_obj)$eir
    yy[i] = E_loc/E
  }

  xds_obj$data_obj$yy <- yy

  return(xds_obj)
}
