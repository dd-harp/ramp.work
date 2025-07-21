
#' Title
#'
#' @param model a **`ramp.xds`** model object
#'
#' @returns a **`ramp.xds`** model object
#' @export
get_predicted_pr = function(model){
  UseMethod("get_predicted_pr", model$fitting$pr_diagnostic)
}

#' Title
#'
#' @param model a **`ramp.xds`** model object
#'
#' @returns a **`ramp.xds`** model object
#' @export
get_predicted_pr.true = function(model){
  pr <- get_XH(model)$true_pr
  return(pr)
}

#' Title
#'
#' @param model a **`ramp.xds`** model object
#'
#' @returns a **`ramp.xds`** model object
#' @export
get_predicted_pr.lm = function(model){
  XH <- get_XH(model)$pfpr_by_lm
}

#' Title
#'
#' @param model a **`ramp.xds`** model object
#'
#' @returns a **`ramp.xds`** model object
#' @export
get_predicted_pr.rdt = function(model){
  XH <- get_XH(model)$pfpr_by_rdt
}
