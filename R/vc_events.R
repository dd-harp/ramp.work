
#' Set Up IRS Evaluation
#'
#' @param xds_obj a **`ramp.xds`**  model object
#' @param jdates the julian dates of IRS events
#' @param pesticides the pesticide used
#' @param frac_sprayed the fraction of houses sprayed
#'
#' @returns a **`ramp.xds`**  model object
#'
#' @export
setup_irs_events = function(xds_obj, jdates, pesticides, frac_sprayed){
  N = length(jdates)
  stopifnot(length(pesticides)==N)
  stopifnot(length(frac_sprayed)==N)

  xds_obj$irs_obj$events = list()
  xds_obj$irs_obj$events$N = N
  xds_obj$irs_obj$events$jdate = jdates
  xds_obj$irs_obj$events$type  = pesticides
  xds_obj$irs_obj$events$peak  = frac_sprayed
  xds_obj$irs_obj$events$round = rep(FALSE, N)

  return(xds_obj)
}

#' Show IRS Events
#'
#' @param xds_obj a **`ramp.xds`**  model object
#' @param mn the bottom of the segment
#' @param mx the top of the segment
#' @param clr a color for the line segments
#' @param add if TRUE, add to an existing plot
#'
#' @importFrom graphics points text segments
#'
#' @returns a **`ramp.xds`**  model object
#'
#' @export
show_irs_events = function(xds_obj, mn=0, mx=1, clr="#4686FBFF", add=FALSE){
  if(add==FALSE) show_fit(xds_obj)
  with(xds_obj$irs_obj$events,{
    for(i in 1:N){
      if(jdate[i]>0){
        points(mx, jdate[i])
        segments(jdate[i], mn, jdate[i], mx, col = clr)
        label = paste(i, "-", type[i])
        text(jdate[i], .8*mx, label, pos=2, srt=90, col = clr)
      }
    }
  })
}


#' Set Up Bed Net Evaluation
#'
#' @param xds_obj a **`ramp.xds`**  model object
#' @param jdates the julian dates of bed net mass distribution events
#' @param net_type the type of net used
#' @param peak_access the fraction of the population with access to a bed net
#'
#' @returns a **`ramp.xds`**  model object
#'
#' @export
setup_bednet_events = function(xds_obj, jdates, net_type, peak_access){
  N = length(jdates)
  stopifnot(length(net_type)==N)
  stopifnot(length(peak_access)==N)

  xds_obj$bednet_obj$events = list()
  xds_obj$bednet_obj$events$N = N
  xds_obj$bednet_obj$events$jdate = jdates
  xds_obj$bednet_obj$events$type  = net_type
  xds_obj$bednet_obj$events$peak  = peak_access
  xds_obj$bednet_obj$events$sim   = rep(FALSE, N)

  return(xds_obj)
}

#' Show bednet Events
#'
#' @param xds_obj a **`ramp.xds`**  model object
#' @param mn the bottom of the segment
#' @param mx the top of the segment
#' @param clr a color for the line segments
#' @param add if TRUE, add to an existing plot
#'
#' @importFrom graphics points text segments
#'
#' @returns a **`ramp.xds`**  model object
#'
#' @export
show_bednet_events = function(xds_obj, mn=0, mx=1, clr="#E4460AFF", add=FALSE){
  if(add==FALSE) show_fit(xds_obj)
  with(xds_obj$bednet_obj$events,{
    for(i in 1:N){
      if(jdate[i]>0){
        points(peak[i]*mx, jdate[i], pch = 19, col = clr)
        segments(jdate[i], mn, jdate[i], mx, col = clr)
        label = paste(i, "-", type[i])
        text(jdate[i], .1*mx, label, pos=4, srt=90, col = clr)
      }
    }
  })
}

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


