

#' Get Spline \eqn{t, y} Values
#'
#' @param xds_obj a **`ramp.xds`**  model object
#'
#' @returns a **`ramp.xds`**  model object
#'
#' @export
fitting_get_spline_ty = function(xds_obj){
  yy <- xds_obj$data$yy
  tt <- xds_obj$data$tt
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
fitting_replace_spline_ty = function(yy, tt, xds_obj){
  stopifnot(length(yy) == length(tt))
  xds_obj$data$yy = yy
  xds_obj$data$tt = tt
  xds_obj$data$n_ty = length(yy)
  xds_obj <- update_fitting_ty(xds_obj)
  xds_obj <- setup_trend_par(xds_obj)
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
fitting_replace_spline_t = function(new_t, xds_obj){
  xds_obj$data$tt = new_t
  xds_obj$data$yy = F_trend(new_t, xds_obj)
  xds_obj <- update_fitting_ty(xds_obj)
  xds_obj <- setup_trend_par(xds_obj)
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
fitting_replace_spline_y = function(new_y, xds_obj){
  stopifnot(length(new_y) == length(xds_obj$data$tt))
  xds_obj$data$yy = new_y
  xds_obj <- update_fitting_ty(xds_obj)
  xds_obj <- setup_trend_par(xds_obj)
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
fitting_change_spline_t = function(new_t, t_ix, xds_obj){
  stopifnot(length(new_t) == length(t_ix))
  xds_obj$data$tt[t_ix] = new_t
  xds_obj$data$yy = F_trend(new_t, xds_obj)
  xds_obj <- update_fitting_ty(xds_obj)
  xds_obj <- setup_trend_par(xds_obj)
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
fitting_change_spline_y = function(new_y, y_ix, xds_obj){
  stopifnot(length(new_y) == length(y_ix))
  xds_obj$data$yy[y_ix] = new_y
  xds_obj <- update_fitting_ty(xds_obj)
  xds_obj <- setup_trend_par(xds_obj)
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
fitting_change_spline_ty = function(new_y, new_t, ix, xds_obj){
  stopifnot(length(new_y) == length(ix))
  stopifnot(length(new_t) == length(ix))
  xds_obj$data$yy[ix] = new_y
  xds_obj$data$tt[ix] = new_t
  xds_obj <- update_fitting_ty(xds_obj)
  xds_obj <- setup_trend_par(xds_obj)
  return(xds_obj)
}
