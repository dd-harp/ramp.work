#' Compute GoF
#'
#' @param xds_obj a **`ramp.xds`** model object
#'
#' @returns sum of squared differences
#' @export
compute_gof = function(xds_obj){
  UseMethod("compute_gof", xds_obj$fitting$gof)
}

#' Compute GoF by LSS
#'
#' @param xds_obj a **`ramp.xds`** model object
#'
#' @returns sum of squared differences
#' @export
compute_gof.sse = function(xds_obj){
  times = c(xds_obj$fitting$t_neg_inf, xds_obj$data$jdates)
  xds_obj <- xds_solve(xds_obj, times=times)
  pred_pr <- get_PR(xds_obj, xds_obj$fitting$pr_diagnostic)[-1]
  sum((xds_obj$data$pfpr-pred_pr)^2)
}

#' Compute the GoF for `X`
#'
#' @param X new parameter values
#' @param xds_obj a **`ramp.xds`** model object
#' @param update a string to dispatch
#' @param ix indices or a list
#'
#' @returns sum of squared differences
#' @export
compute_gof_X = function(X, xds_obj, update, ix){
  xds_obj <- update_function_X(X, xds_obj, update, ix)
  compute_gof(xds_obj)
}

#' Get Initial Values for Parameters
#'
#' @param xds_obj a **`ramp.xds`** model object
#' @param update the dispatch string
#' @param ix the index
#'
#' @return a list with the mean peak and the values
#' @export
get_init_X <- function(xds_obj, update, ix=1){
  if(length(update)>1) class(update) = "multifit"
  else class(update) = update
  UseMethod("get_init_X", update)
}

#' Update a function
#'
#' @param X new parameter values
#' @param xds_obj a **`ramp.xds`** xds_obj object
#' @param update a string to dispatch
#' @param ix indices or a list
#'
#' @returns sum of squared differences
#' @export
update_function_X = function(X, xds_obj, update, ix=1){
  if(length(update)>1) class(update) = "multifit"
  else class(update) = update
  UseMethod("update_function_X", update)
}


#' Get Limits
#'
#' @param xds_obj a **`ramp.xds`** model object
#' @param update the dispatch string
#'
#' @return a list with the mean peak and the values
#' @export
get_limits_X <- function(xds_obj, update){
  class(update) <- update
  UseMethod("get_limits_X", update)
}
