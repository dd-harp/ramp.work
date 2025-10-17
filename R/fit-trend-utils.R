

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
  ev1 <- xds_obj$events_obj$bednet$jdate
  ev1 <- ev1[ev1>-60]
  ev2 <- xds_obj$events_obj$irs$jdate
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
#'
#' @returns numeric
#' @export
time_since_event = function(xds_obj){
  N = length(xds_obj$data$tt)
  last = rep(0, N)
  for(i in 2:N){
    delta = xds_obj$data$tt[i] - c(xds_obj$events_obj$bednet$jdate, xds_obj$events_obj$irs$jdate)
    last[i] = min(delta[delta>0])
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
  jdate = xds_obj$events_obj$irs$jdate[round_ix]
  delta = xds_obj$data$tt - jdate
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
  jdate = xds_obj$events_obj$bednet$jdate[round_ix]
  delta = xds_obj$data$tt - jdate
  return(which(delta>0)[1:N])
}
