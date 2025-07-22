#' @title Burn-in
#'
#' @description Use the defined hindcast to
#' run a model from the past up to the present.
#' Reset the initial conditions.
#'
#' @param model a **`ramp.xds`** model object
#'
#' @returns a **`ramp.xds`** model object
#'
#' @export
burnin = function(model){
  times = c(model$hindcast$tt, 0)
  model <- xds_solve(model, times=times)
  model <- last_to_inits(model)
  return(model)
}
