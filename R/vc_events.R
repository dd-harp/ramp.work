


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
 ev1 <- xds_obj$bednet_obj$events$jdate
 ev1 <- ev1[ev1>-60]
 ev2 <- xds_obj$irs_obj$events$jdate
 ev2 <- ev2[ev2>-60]
 stopifnot(length(ev1) + length(ev2) >0)
 if(length(ev1) ==0) ev1 = c(0)
 if(length(ev2) ==0) ev2 = c(0)
 tt <- sort(c(0, ev1-b4, ev1+fu, ev2-b4, ev2+fu))
 tt = tt[which(tt>=0)]
 ix = which(diff(tt)<mngp)
 tt = sort(tt[-ix])
 gap = diff(tt)
 gix = which(gap > mxgp)
 tt = c(tt, tt[gix] + gap[gix]/2)
 ix = which(diff(tt)<mngp)
 if(length(ix)>0) tt = tt[-ix]
 return(sort(tt))
}


