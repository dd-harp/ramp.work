
#' Mask data
#'
#' @description Mask a subset of data so it
#' is not used for fitting
#'
#' @param xds_obj a **`ramp.xds`**  model object
#' @param mask_ix the indices of the observations to mask
#'
#' @return an **`xds_obj`**
#'
#' @export
mask_data = function(xds_obj, mask_ix){
  og <- with(xds_obj$data_obj, exists("original_pfpr"))
  if(!og) xds_obj$data_obj$original_pfpr = xds_obj$data_obj$pfpr
  if(!og) xds_obj$data_obj$original_jdates = xds_obj$data_obj$jdates

  xds_obj$data_obj$mask_ix = mask_ix
  xds_obj$data_obj$pfpr = xds_obj$data_obj$original_pfpr[-mask_ix]
  xds_obj$data_obj$jdates = xds_obj$data_obj$original_jdates[-mask_ix]
  return(xds_obj)
}

#' Unmask data
#'
#' @description
#' Restore the full data from the stored original
#'
#' @param xds_obj a **`ramp.xds`**  model object
#'
#' @return an **`xds_obj`**
#'
#' @export
unmask_data = function(xds_obj){
  xds_obj$data_obj$pfpr = xds_obj$data_obj$original_pfpr
  xds_obj$data_obj$jdates = xds_obj$data_obj$original_jdates
  return(xds_obj)
}

#' Mask data around events
#'
#' @description Mask the data around events, parameters control
#' the number of days before (negative values are OK) and
#' after.
#'
#' @param xds_obj a **`ramp.xds`**  model object
#' @param daysb4 the number of days before the event to mask
#' @param days_after the number of days after the event to mask
#'
#' @return an **`xds_obj`**
#'
#' @export
mask_events = function(xds_obj, daysb4=0, days_after=365){
  st = xds_obj$events_obj$bednet$start_day
  if(with(xds_obj$events_obj, exists("irs"))){
    irs_st = xds_obj$events_obj$irs$start_day
    st = c(st, irs_st)
  }
  jdates = xds_obj$data_obj$jdate
  mask_ix = c()
  for(i in 1:length(st)){
    t0 = st[i]-daysb4
    t1 = st[i]+days_after
    ix = which(jdates>t0 & jdates<t1)
    mask_ix = c(mask_ix, ix)
  }
  mask_ix = unique(mask_ix)
  xds_obj <- mask_data(xds_obj, mask_ix)
  return(xds_obj)
}

#' Mask data around a bednet event
#'
#' @param i the index of the event to mask
#' @param xds_obj a **`ramp.xds`**  model object
#' @param daysb4 the number of days before the event to mask
#' @param days_after the number of days after the event to mask
#'
#' @export
mask_bn_event = function(i, xds_obj, daysb4=0, days_after=365){
  st = xds_obj$events_obj$bednet$start_day[i]
  jdates = xds_obj$data_obj$jdate
  t0 = st-daysb4
  t1 = st+days_after
  mask_ix = which(jdates>t0 & jdates<t1)
  xds_obj <- mask_data(xds_obj, mask_ix)
  return(xds_obj)
}
