

#' Adjust Spacing
#'
#' @param xds_obj a **`ramp.xds`**  model object
#' @param b4 days before intervention
#' @param fu days after intervention
#' @param mngp minimum gap
#' @param mxgp the maximum gap
#'
#' @returns a **`ramp.xds`**  model object
#'
#' @export
event_chop_spline_t = function(xds_obj, b4=30, fu=210, mngp=90, mxgp=500){
  ev1 <- xds_obj$events_obj$bednet$start_day
  ev1 <- ev1[ev1>-60]
  ev2 <- xds_obj$events_obj$irs$start_day
  ev2 <- ev2[ev2>-60]
  if(length(ev1) + length(ev2)==0) return(xds_obj$data_obj$tt)
  if(length(ev1) ==0) ev1 = c(0)
  if(length(ev2) ==0) ev2 = c(0)
  tt <- sort(c(0, ev1-b4, ev1+fu, ev2-b4, ev2+fu))
  tt = tt[which(tt>=0)]
  ix = which(diff(tt)<mngp)
  if(length(ix)>0)
    tt = sort(tt[-ix])
  gap = diff(tt)
  gix = which(gap > mxgp)
  if(length(gix)>0)
    tt = c(tt, tt[gix] + gap[gix]/2)
  ix = which(diff(tt)<mngp)
  if(length(ix)>0) tt = tt[-ix]
  return(sort(tt))
}


#' @title Time Since Event
#'
#' @description Compute the time elapsed
#' since the last vector control event
#' for all the spline \eqn{t}-values
#'
#' @param xds_obj a **`ramp.xds`** model object
#' @param bednet_ix list of events to fit; if empty, then fit all
#' @param irs_ix list of events to fit; if empty, then fit all
#'
#' @returns numeric
#' @export
time_since_event = function(xds_obj, bednet_ix = list(), irs_ix=list()){

  if(with(xds_obj$events_obj, exists("bednet")))
    if(length(bednet_ix)==0)
      bednet_ix = 1:xds_obj$events_obj$bednet$N

  start_days = c()
  if(length(bednet_ix)>0) start_days = xds_obj$events_obj$bednet$start_day[bednet_ix]

  if(with(xds_obj$events_obj, exists("irs")))
    if(length(irs_ix)==0)
      irs_ix = 1:xds_obj$events_obj$irs$N

  if(length(irs_ix)>0) start_days = c(start_days, xds_obj$events_obj$irs$start_day[irs_ix])


  N = length(xds_obj$data_obj$tt)
  last = rep(1e10, N)
  for(i in 1:N){
    delta = xds_obj$data_obj$tt[i] - start_days
    last[i] <- min(delta[delta>0], last[i])
  }

  return(last)
}

#' @title Time Since Event
#'
#' @description Compute the time elapsed
#' since the last vector control event
#' for all the spline \eqn{t}-values
#'
#' @param round_ix the index of the IRS round
#' @param xds_obj a **`ramp.xds`** model object
#' @param N the number of y indices
#'
#' @returns numeric
#' @export
get_yix_after_irs_round = function(round_ix, xds_obj, N=1){
  start_day = xds_obj$events_obj$irs$start_day[round_ix]
  delta = xds_obj$data_obj$tt - start_day
  return(which(delta>0)[1:N])
}

#' @title Time Since Event
#'
#' @description Compute the time elapsed
#' since the last vector control event
#' for all the spline \eqn{t}-values
#'
#' @param round_ix the index of the IRS round
#' @param xds_obj a **`ramp.xds`** model object
#' @param N the number of y indices
#'
#' @returns numeric
#' @export
get_yix_after_bednet_round = function(round_ix, xds_obj, N=1){
  start_day = xds_obj$events_obj$bednet$start_day[round_ix]
  delta = xds_obj$data_obj$tt - start_day
  return(which(delta>0)[1:N])
}
