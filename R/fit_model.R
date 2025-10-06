#' Fit a model to data
#' @description
#' A function to fit multiple feature of a model at the same time.
#'
#' The feature of a model to be fit are passed as text
#' strings in a list, called `feature.`
#'
#' For some of these feature, `fit_model`
#' can be configured to fit only some of the parameters or
#' to lump some parameters together (*e.g.* IRS rounds that
#' use the same pesticide). Each feature sets its own
#' default for indexing the subset of parameters to be fit,
#' but these can be overridden by passing
#' text strings in `options.`
#'
#' If `length(feature) == 1` then there is another `fit_`
#' function in `ramp.work` that is equivalent to
#' `fit_model` where parameter constraints and indexing options
#' are described in greater detail.
#' These are linked in the bullet points below.
#'
#' If `length(feature) > 1` then `class(feature)= "multifit"` and
#' each feature in the list gets handled separately.
#'
#' The function acts only on the following text strings in `feature`:
#' + **`mean_forcing`** : fits the mean forcing parameter (also, see [fit_mean_forcing])
#' + **`trend`** : fits interpolation points for a spline function (also, see [fit_trend])
#'    - by default, all \eqn{y} values of interpolation points are fitted
#'    - a subset can be configured by setting `trend_ix` in `options`
#'    - if `trend`  and `mean_forcing` are both called, at least one of the interpolation points should be set to `1`
#' + At most one of the following aspects of seasonality:
#'    - **`phase`** : the time of year when forcing peaks (also, see [fit_season_phase])
#'    - **`pw`** : a shape parameter affecting the amplitude (also, see [fit_season_pw])
#'    - **`bottom`** : a shape parameter affecting amplitude (also, see [fit_season_bottom])
#'    - **`amplitude`** : fits the shape parameters that affect amplitude: `pw` & `bottom` (also, see [fit_season_amplitude])
#'    - **`season`** : fits all seasonality shape parameters: `pw` & `bottom` & `phase` (also, see [fit_season])
#' + **`irs_coverage`** : fit irs coverage parameters (also see [fit_irs_coverage])
#'    - by default, coverage levels for all rounds are fitted independently
#'    - other options can be fitted by setting `irs_ix` in `options`
#' + **`bednet_contact`** : fit bed net coverage parameters (also see [fit_bednet_contact])
#'    - by default, coverage levels for all rounds are fitted independently
#'    - other options can be fitted by setting `bednet_ix` in `options`
#'
#' The **`xds`** model object that gets returned by `fit_model` has the optimal paramters.
#'
#' @param xds_obj an **`xds`** model object
#' @param feature a list of one or more model feature to fit
#' @param options a list of options to override feature-specific defaults
#'
#' @return an **`xds`** model object
#' @export
fit_model <- function(xds_obj, feature, options=list()){

  if(length(feature)>1) class(feature) = "multifit"

  options$max_ix = 0
  for(i in 1:length(feature))
    options <- setup_fitting_indices(xds_obj, feature[i], options)


  Xinits = get_init_X(xds_obj, feature, options)
  inits = unlist(Xinits)
  xds_obj$fitting$Xinits = Xinits

  if(length(Xinits)==1){
    lims = get_limits_X(xds_obj, feature)
    fitit <- stats::optimize(compute_gof_X, lims, feature=feature,
                             xds_obj=xds_obj, options=options)
    X <- fitit$minimum
  } else {
    fitit <- stats::optim(inits, compute_gof_X, feature=feature,
                          options=options, xds_obj=xds_obj)
    X <- fitit$par
  }
  xds_obj <- update_function_X(X, xds_obj, feature, options)

  xds_obj <- burnin(xds_obj)
  return(xds_obj)
}

#' Set up indices for model fitting
#'
#' @param xds_obj an **`xds`** model object
#' @param feature a list of model features to feature
#' @param options options for setting indices
#'
#' @returns options
#'
#' @export
#'
setup_fitting_indices = function(xds_obj, feature, options){
  class(feature) <- feature
  UseMethod("setup_fitting_indices", feature)
}

#' Compute the GoF for `X`
#'
#' @inheritParams get_init_X
#'
#' @returns sum of squared differences
#' @export
get_init_X.multifit = function(xds_obj, feature, options=list()){
  Xinits = list()
  for(i in 1:length(feature))
    Xinits[[i]] = get_init_X(xds_obj, feature[i], options)
  return(Xinits)
}

#' Compute the GoF for `X`
#'
#' @inheritParams compute_gof_X
#'
#' @returns sum of squared differences
#' @export
update_function_X.multifit = function(X, xds_obj, feature, options=list()){
  for(i in 1:length(feature))
    xds_obj <- update_function_X(X, xds_obj, feature[i], options)
  return(xds_obj)
}


#' @title Plot the model and the data
#'
#' @param xds_obj a **`ramp.xds`** model object
#' @param clr the color of the line to be plotted
#' @param add add to an existing show_fit plot
#'
#' @returns the PR, invisibly
#' @export
show_fit = function(xds_obj, clr="black", add=FALSE){
  gof <- round(compute_gof(xds_obj)*1e5)/1e5
  if(add==FALSE)
    with(xds_obj$data, plot(jdates, pfpr, type = "p", xlab = "Time", ylab = "PfPR", pch=15, main = gof, xlim = range(0, jdates), ylim = range(0, pfpr)))
  xds_obj <- ramp.xds::xds_solve(xds_obj, times = c(-3650, xds_obj$data$jdates[1]))
  xds_obj <- last_to_inits(xds_obj)
  xds_obj <- ramp.xds::xds_solve(xds_obj, times = xds_obj$data$jdates)
  ramp.xds::xds_plot_PR(xds_obj, clr=clr, add=TRUE)
  return(invisible(get_PR(xds_obj)))
}

#' @title Plot the model and the data
#'
#' @param xds_obj a **`ramp.xds`** model object
#' @param clr the color of the line to be plotted
#' @param add add to an existing show_fit plot
#' @importFrom graphics segments
#'
#' @returns the PR, invisibly
#' @export
show_residuals = function(xds_obj, clr="black", add=FALSE){
  jdates = xds_obj$data$jdates
  xds_obj <- ramp.xds::xds_solve(xds_obj, times = c(0, jdates))
  residuals <- xds_obj$data$pfpr - get_PR(xds_obj, 1)[-1]
  if(add==FALSE)
    plot(jdates, residuals, type = "p", xlab = "Time", ylab = "PfPR", pch=15, ylim = range(-max(residuals), max(residuals)))
  segments(0, 0, max(jdates), 0)
  return(invisible(residuals))
}




