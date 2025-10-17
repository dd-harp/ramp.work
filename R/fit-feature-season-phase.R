
#' @title Fit a seasonal pattern
#'
#' @description For a *Pf*PR time series, \eqn{x}, fit
#' a spline function
#'
#' @param xds_obj an `xds` xds_obj
#'
#' @return a list with the mean peak and the values
#' @export
fit_season_phase <- function(xds_obj){

  options=list()
  options$max_ix = 0
  options <- setup_fitting_indices(xds_obj, "phase", options)

  phase = get_init_X(xds_obj, "phase")
  lims = get_limits_X(xds_obj, "phase")


  fitit <- stats::optimize(compute_gof_X, lims,
                           xds_obj=xds_obj, options=options, feature = c("phase"))

  X <- fitit$minimum
  xds_obj <- update_function_X(X, xds_obj, "phase", options)
  xds_obj <- burnin(xds_obj)
  return(xds_obj)
}

#' Seasonality: `phase` Indices
#'
#' @inheritParams setup_fitting_indices
#'
#' @returns options
#'
#' @export
#'
setup_fitting_indices.phase = function(xds_obj, feature, options){

  options$phase_ix = 1
  options$phase_ixX = options$max_ix + 1:length(options$phase_ix)
  options$max_ix = max(options$phase_ixX)

  return(options)
}

#' Get Initial Values for Parameters
#'
#' @inheritParams get_init_X
#'
#' @return a vector
#' @export
get_init_X.phase <- function(xds_obj, feature, options=list()){
  return(list(phase = get_season(xds_obj)$phase))
}

#' Get Initial Values for Parameters
#'
#' @inheritParams get_limits_X
#'
#' @return a vector
#' @export
get_limits_X.phase <- function(xds_obj, feature="phase"){
  return(c(0,365))
}

#' feature a function
#'
#' @inheritParams update_function_X
#'
#' @returns sum of squared differences
#' @export
update_function_X.phase = function(X, xds_obj, feature, options){
  phase <- get_season_phase(xds_obj, 1)
  phase   <- with(options, modify_vector_X(phase, phase_ix, X, phase_ixX))
  xds_obj <- change_season(list(phase=phase), xds_obj, 1)
  return(xds_obj)
}

#' @title Fit the phase
#'
#' @description For a *Pf*PR time series, \eqn{x}, fit
#' the phase for a seasonal pattern. The algorithm works
#' by choosing an initial mesh, around 46 days, and then
#' shrinking the mesh around the value that maximizes
#' the goodness of fit. The \eqn{n} parameter determines
#' how many times the process is iterated. For \eqn{n=2},
#' the mesh is
#'
#' @param xds_obj an `xds` xds_obj
#' @param n number of times to iterate
#'
#' @return a list with the mean peak and the values
#' @export
fit_season_phase_alt <- function(xds_obj, n=2){

  phase <- seq(0, 365, length.out=9)[-1]
  gof = phase*0
  for(i in 1:8) gof[i] = compute_gof_X(phase[i], xds_obj, "phase", 1)

  ix = which.min(gof)
  ff <- gof[ix]
  best <- phase[ix]

  for(iter in c(1:n)){
    df = max(diff(phase))
    phase <- seq(best-df, best+df, length.out=9)[-1]
    gof = phase*0
    for(i in 1:8){
      gof[i] = compute_gof_X(phase[i], xds_obj, "phase", 1)
    }
    ix = which.min(gof)
    ff <- gof[ix]
    best <- phase[ix]
  }

  xds_obj <- update_function_X(best, xds_obj, "phase", 1)
  xds_obj <- burnin(xds_obj)
  return(xds_obj)
}

