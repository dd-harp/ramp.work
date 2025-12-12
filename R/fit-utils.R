#' @title Sigmoid-X
#'
#' @description A sigmoid function to
#' bound parameters
#'
#' @param x a number
#' @param mn the asymptotic value at negative infinity
#' @param mx the asymptotic value at infinity
#'
#' @returns a number
#' @export
sigX = function(x, mn=0, mx=1){
  mn + (mx-mn)*exp(x)/(1+exp(x))
}

#' @title Sigmoid-X Inverse
#'
#' @description The inverse of [sigX]
#'
#' @param x a number
#' @param mn the asymptotic value at negative infinity
#' @param mx the asymptotic value at infinity
#'
#' @returns a number
#' @export
sigXinv = function(x, mn=0, mx=1){
  stopifnot(x<=mx)
  stopifnot(x>=mn)
  if(x==mx) x = mx-1e-5
  if(x==mn) x = mn+1e-5
  log((x-mn)/(mx-x))
}

#' @title Normalize Trend
#'
#' @description Adjust the y-values for the
#' interpolation point and reset the mean
#' forcing parameter such that the product
#' of the seasonal pattern and the trend over
#' an observation period is one.
#'
#' @param xds_obj a **`ramp.xds`** object
#'
#' @returns a number
#' @export
norm_trend = function(xds_obj){
  UseMethod("norm_trend", xds_obj$forced_by)
}

#' @title Normalize Trend
#'
#' @description Adjust the y-values for the
#' interpolation point and reset the mean
#' daily EIR such that the product
#' of the seasonal pattern and the trend over
#' an observation period is one.
#'
#' @param xds_obj a **`ramp.xds`** object
#' @keywords internal
#' @importFrom stats integrate
#' @returns a number
#' @export
norm_trend.eir = function(xds_obj){
  fS = make_function(xds_obj$EIR_obj$season_par)
  fT = make_function(xds_obj$EIR_obj$trend_par)
  F = function(t){fS(t)*fT(t)}
  t0 = min(xds_obj$data_obj$jdate)
  tX = max(xds_obj$data_obj$jdate)
  mean = integrate(F, t0, tX)$value/(tX-t0)
  xds_obj$EIR_obj$eir = xds_obj$EIR_obj$eir*mean
  xds_obj$data_obj$yy = xds_obj$data_obj$yy/sqrt(mean)
  xds_obj <- update_fit_trend(xds_obj)
  return(xds_obj)
}

#' @title Normalize Trend
#'
#' @description Adjust the y-values for the
#' interpolation point and reset the mean
#' daily adult emergence rate such that the product
#' of the seasonal pattern and the trend over
#' an observation period is one.
#'
#' @param xds_obj a **`ramp.xds`** object
#'
#' @importFrom stats integrate
#' @returns a number
#' @export
norm_trend.Lambda = function(xds_obj){
  fS = make_function(xds_obj$L_obj$season_par)
  fT = make_function(xds_obj$L_obj$trend_par)
  F = function(t){fS(t)*fT(t)}
  t0 = min(xds_obj$data_obj$jdate)
  tX = max(xds_obj$data_obj$jdate)
  mean = integrate(F, t0, tX)$value/(tX-t0)
  xds_obj$L_obj$Lambda = xds_obj$L_obj$Lambda*mean
  xds_obj$data_obj$yy = xds_obj$data_obj$yy/sqrt(mean)
  xds_obj <- update_fit_trend(xds_obj)
  return(xds_obj)
}
