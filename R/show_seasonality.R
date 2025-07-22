
#' Plot the Fitted season
#'
#' @param model a **`ramp.xds** model object
#' @param dt the outputs interval
#' @param add add to existing plot
#'
#' @returns a **`ramp.xds** model object
#' @export
show_season = function(model, dt=10, add=FALSE){
  UseMethod("show_season", model$fitting)
}

#' Plot the Fitted season
#'
#' @description for a model forced by emergence,
#' output the fitted season
#'
#' @inheritParams show_season
#'
#' @returns a **`ramp.xds** model object
#' @importFrom graphics plot lines
#' @export
show_season.Lambda = function(model, dt=10, add=FALSE){
  tt <- with(model$fitting, seq(min(tt), max(tt), by = dt))
  season <- model$Lpar[[1]]$F_season(tt)
  if(add==FALSE) plot(tt, season, type = "n")
  lines(tt, season)
}

#' Plot the Fitted season
#'
#' @description for a model forced by emergence,
#' output the fitted season
#'
#' @inheritParams show_season
#'
#' @returns a **`ramp.xds** model object
#' @export
show_season.eir= function(model, dt=10, add=FALSE){
  tt <- with(model$fitting, seq(min(tt), max(tt), by = dt))
  season <- model$EIRpar$F_season(tt)
  if(add==FALSE) plot(tt, season, type = "n")
  lines(tt, season)
}
