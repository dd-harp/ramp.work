
#' Plot the Fitted Trend
#'
#' @param model a **`ramp.xds** model object
#' @param dt the outputs interval
#' @param add add to existing plot
#'
#' @returns a **`ramp.xds** model object
#' @export
show_trend = function(model, dt=10, add=FALSE){
  UseMethod("show_trend", model$fitting)
}

#' Plot the Fitted Trend
#'
#' @description for a model forced by emergence,
#' output the fitted trend
#'
#' @inheritParams show_trend
#'
#' @returns a **`ramp.xds** model object
#' @export
show_trend.Lambda = function(model, dt=10, add=FALSE){
  tt <- with(model$fitting, seq(min(tt), max(tt), by = dt))
  trend <- model$Lpar[[1]]$F_trend(tt)
  if(add==FALSE) plot(tt, trend, type = "n")
  lines(tt, trend)
}

#' Plot the Fitted Trend
#'
#' @description for a model forced by emergence,
#' output the fitted trend
#'
#' @inheritParams show_trend
#'
#' @returns a **`ramp.xds** model object
#' @export
show_trend.eir= function(model, dt=10, add=FALSE){
  tt <- with(model$fitting, seq(min(tt), max(tt), by = dt))
  trend <- model$EIRpar$F_trend(tt)
  if(add==FALSE) plot(tt, trend, type = "n")
  lines(tt, trend)
}
