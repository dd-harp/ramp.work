
#' Show Events
#'
#' @param xds_obj a **`ramp.xds`**  model object
#' @param mn the bottom of the segment
#' @param mx the top of the segment
#' @param bclr a color for the bednet line segments
#' @param iclr a color for the irs line segments
#' @param add if TRUE, add to an existing plot
#' @importFrom graphics points text
#' @returns a **`ramp.xds`**  model object
#'
#' @export
show_events = function(xds_obj, mn=0, mx=1, bclr="#E4460AFF", iclr = "#4686FBFF", add=FALSE){
  show_bednet_events(xds_obj, mn, mx, bclr)
  show_irs_events(xds_obj, mn, mx, iclr, add=TRUE)
  xds_obj$data$tt -> tt
  xds_obj$data$yy -> yy
  points(tt, 0*yy, pch=10, cex=1.5, col ="#30123BFF")
  text(tt, 0*yy, 1:length(tt), pos=3, col = "#30123BFF")
}

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


