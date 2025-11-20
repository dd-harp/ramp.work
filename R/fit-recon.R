
#' @title Reconstruct a history of exposure from a PR time series
#'
#' @description Construct a function describing the EIR, including
#' the mean EIR, the seasonal pattern, and the i
#'
#' For set of paired a time series \eqn{X,} compute the
#' phase of a seasonal pattern for the EIR
#'
#' @param xds_obj an `xds` xds_obj
#' @param do_bednet if TRUE then fit bednet shocks
#' @param bednet_ix indices of events to fit
#' @param do_irs if TRUE then fit irs shocks
#' @param irs_ix indices of events to fit
#' @param D remove spline points less than D days after an event
#' @param N cycle through fits N times
#'
#' @return the **`ramp.xds`** model object, fitted to a time series.
#' The state at the end is saved as `xds_obj$history`
#' @export
reconD_shock_xm = function(xds_obj, do_bednet=TRUE, bednet_ix=c(), do_irs=TRUE, irs_ix=c(), D=365, N=2){

  options = list()
  if(do_bednet == TRUE){

    if(with(xds_obj$events_obj, exists("bednet")))
      if(length(bednet_ix)==0)
        bednet_ix = 1:xds_obj$events_obj$bednet$N

    xds_obj <- setup_bednet_coverage("multiround", xds_obj)
    xds_obj <- setup_F_multishock(xds_obj)
    options$bednet_shock_ix = bednet_ix
  } else {
    bednet_ix = 0
  }

  if(do_irs == TRUE){

    if(with(xds_obj$events_obj, exists("irs")))
      if(length(irs_ix)==0)
        irs_ix = 1:xds_obj$events_obj$irs$N

    xds_obj <- setup_irs_coverage("multiround", xds_obj)
    xds_obj <- setup_F_multishock(xds_obj)
    options$irs_shock_ix = irs_ix
  } else {
    irs_ix = 0
  }

  lag_event <- time_since_event(xds_obj, bednet_ix, irs_ix)
  ix = which(lag_event<D)

  stopifnot(length(ix)>0)
#  print(paste("Removing spline points:", ix))
  xds_obj <- rm_ix_fit_spline_ty(ix, xds_obj)


  if(do_bednet) xds_obj <- fit_bednet_shock(xds_obj, options=options)
  if(do_bednet) xds_obj <- fit_bednet_shock_d50(xds_obj, options=options)
  if(do_irs) xds_obj <- fit_irs_shock(xds_obj, options=options)

  for(i in 1:N){
    xds_obj <- fit_trend(xds_obj)
    if(do_bednet) xds_obj <- fit_bednet_shock_d50(xds_obj, options=options)
    if(do_bednet) xds_obj <- fit_bednet_shock(xds_obj, options=options)
    if(do_irs) xds_obj <- fit_irs_shock(xds_obj, options=options)
  }

  return(xds_obj)
}
