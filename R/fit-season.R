
#' Check the seasonal setup
#'
#' @param xds_obj a **`ramp.xds`** model object
#'
#' @return a **`ramp.xds`** model object
#' @export
check_season_par = function(xds_obj){
  UseMethod("check_season_par", xds_obj$forced_by)
}

#' Check the seasonal setup
#'
#' @param xds_obj a **`ramp.xds`** model object
#'
#' @return a **`ramp.xds`** model object
#' @export
check_season_par.Lambda= function(xds_obj){
  Sp <- init_fit_season(xds_obj$L_obj[[1]]$season_par)
  xds_obj$L_obj[[1]]$season_par <- Sp
  xds_obj$L_obj[[1]]$F_season <- make_function(Sp)
  return(xds_obj)
}

#' Check the seasonal setup
#'
#' @param xds_obj a **`ramp.xds`** model object
#'
#' @return a **`ramp.xds`** model object
#' @export
check_season_par.eir = function(xds_obj){
  Sp <- init_fit_season(xds_obj$EIR_obj$season_par)
  xds_obj$EIR_obj$season_par <- Sp
  xds_obj$EIR_obj$F_season <- make_function(Sp)
  return(xds_obj)
}

#' Set up the seasonal pattern
#'
#' @param season_par seasonal pattern parameters
#'
#' @return a `sin` function parameter set
#' @export
#' @seealso [makepar_F_sin()]
init_fit_season = function(season_par){
  UseMethod("init_fit_season", season_par)
}

#' Set up the seasonal pattern
#'
#' @description
#' If the current `season_par` is a list,
#' return a parameter set
#'
#' @param season_par seasonal pattern parameters
#'
#' @return a `sin` function parameter set
#'
#' @seealso [makepar_F_sin()]
#'
#' @export
#'
init_fit_season.list = function(season_par){
  makepar_F_sin(bottom=2)
}

#' Set up the seasonal pattern
#'
#' @description
#' If the current `season_par` is from the `sin`
#' family of functions, return the parameters unchanged
#'
#' @param season_par seasonal pattern parameters
#'
#' @return a `sin` function parameter set
#'
#' @seealso [makepar_F_sin()]
#'
#' @export
#'
init_fit_season.sin = function(season_par){
  season_par
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
  xds_obj <- change_season(list(phase=adjust-phase), xds_obj, s=1)
  xds_obj <- xds_solve(xds_obj, times = c(-3650, xds_obj$data$jdates))
  return(xds_obj)
}
