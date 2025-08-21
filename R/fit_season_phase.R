
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

  phase = get_init_X(xds_obj, "phase")
  lims = get_limits_X(xds_obj, "phase")


  fitit <- stats::optimize(compute_gof_X, lims,
                           xds_obj=xds_obj, ix=1, update = c("phase"))

  X <- fitit$minimum
  xds_obj <- update_function_X(X, xds_obj, update = c("phase"))
  xds_obj <- burnin(xds_obj)
  return(xds_obj)
}

#' Get Initial Values for Parameters
#'
#' @inheritParams get_init_X
#'
#' @return a vector
#' @export
get_init_X.phase <- function(xds_obj, update="phase", ix=1){
  return(get_season(xds_obj)$phase[ix])
}

#' Get Initial Values for Parameters
#'
#' @inheritParams get_limits_X
#'
#' @return a vector
#' @export
get_limits_X.phase <- function(xds_obj, update="phase"){
  return(c(0,365))
}

#' Update a function
#'
#' @inheritParams update_function_X
#'
#' @returns sum of squared differences
#' @export
update_function_X.phase = function(X, xds_obj, update="phase", ix=1){
  phase <- get_season_phase(xds_obj, 1)
  phase <- modify_vector_X(X, ix, phase)
  xds_obj <- set_season_phase(phase, xds_obj, 1)
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

#' @title Compute the observed phase
#'
#' @description For a *Pf*PR time series, \eqn{x}, fit
#' a spline function
#'
#' @param pfpr a *Pf*PR time series
#' @param jdate the corresponding dates
#'
#' @return a list with the mean peak and the values
#' @export
approx_phase <- function(pfpr, jdate){
  toy <- seq(5, 365, by=5)
  spr <- 0*toy
  for(i in 1:73){
    ix <- which(abs(jdate%%365 - toy[i]) < 20)
    spr[i]  <- mean(pfpr[ix])
  }
  toy[which.max(spr)]
}

#' @title Initialize the phase parameter
#'
#' @description Compute the empirical phase
#' from a *Pf*PR time series, \eqn{t,x}, and
#' adjust the model phase to match it.
#'
#' @note This algorithm makes a crude guess
#' at the seasonal phase for the forcing function.
#' *close* to the value that
#' will get fitted later.
#'
#' @param xds_obj an `xds` xds_obj
#'
#' @return a list with the mean peak and the values
#' @export
preset_phase <- function(xds_obj){
  data_phase <- with(xds_obj$data, approx_phase(pfpr, jdates))

  times = c(0, xds_obj$data$jdates)
  xds_obj <- xds_solve(xds_obj, times=times)
  model_phase <- approx_phase(get_PR(xds_obj), times)
  adjust = data_phase - model_phase
  phase <- get_season_phase(xds_obj, 1)
  xds_obj <- set_season_phase(phase-adjust, xds_obj, s=1)
  xds_obj <- xds_solve(xds_obj, times = c(0, xds_obj$data$jdates))
  return(xds_obj)
}
