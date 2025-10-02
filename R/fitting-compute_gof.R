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
  pred_pr <- get_PR(xds_obj, i=1, method=xds_obj$fitting$pr_diagnostic)[-1]
  sum((xds_obj$data$pfpr-pred_pr)^2)
}

#' Compute the GoF for `X`
#'
#' @param X new parameter values
#' @param xds_obj a **`ramp.xds`** model object
#' @param feature a string to dispatch a specific model feature
#' @param options a list to configure feature-specific indexing
#'
#' @returns sum of squared differences
#' @export
compute_gof_X = function(X, xds_obj, feature, options=list()){
  xds_obj <- update_function_X(X, xds_obj, feature, options)
  gof <- compute_gof(xds_obj)
  return(gof)
}

#' Get Initial Values for Parameters
#'
#' @param xds_obj a **`ramp.xds`** model object
#' @param feature the dispatch string
#' @param options a list to configure feature-specific indexing
#'
#' @return a list with the mean peak and the values
#' @export
get_init_X <- function(xds_obj, feature, options=list()){
  if(length(feature)>1) class(feature) = "multifit"
  else class(feature) = feature
  UseMethod("get_init_X", feature)
}

#' Update a function
#'
#' @param X new parameter values
#' @param xds_obj a **`ramp.xds`** xds_obj object
#' @param feature a string to dispatch
#' @param options a list to configure feature-specific indexing
#'
#' @returns sum of squared differences
#' @export
update_function_X = function(X, xds_obj, feature, options=list()){
  if(length(feature)>1) class(feature) = "multifit"
  else class(feature) = feature
  UseMethod("update_function_X", feature)
}


#' Get Limits
#'
#' @param xds_obj a **`ramp.xds`** model object
#' @param feature the dispatch string
#'
#' @return a list with the mean peak and the values
#' @export
get_limits_X <- function(xds_obj, feature){
  class(feature) <- feature
  UseMethod("get_limits_X", feature)
}
