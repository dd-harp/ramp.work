
#' @title Scaling for Malaria Metrics
#'
#' @description Compute scaling relationships for malaria metrics
#'
#' This function is a wrapper that dispatches on `xds` to call
#' + [xde_scaling] for differential equations
#' + [dts_scaling] for discrete-time systems
#'
#' @param xds_obj a **`ramp.xds`** model object
#' @param N an integer
#' @param rbr the relative biting rate for a population stratum
#'
#' @return a **`ramp.xds`** model object with a scaling table
#'
#' @export
xds_scaling = function(xds_obj, N=25, rbr=1){
   UseMethod("xds_scaling", xds_obj$xds)
}

#' @title Scaling for Malaria Metrics
#'
#' @description For `class(xds) == xde,` call [xde_scaling].
#'
#' @inheritParams xds_scaling
#'
#' @return a **`ramp.xds`** model object with a scaling table
#'
#' @export
xds_scaling.xde = function(xds_obj, N=25, rbr=1){
  xde_scaling(xds_obj, N, rbr)
}

#' @title Scaling for Malaria Metrics
#'
#' @description For `class(xds) == dts` call [dts_scaling]
#'
#' @inheritParams xds_scaling
#'
#' @return a **`ramp.xds`** model object with a scaling table
#'
#' @export
xds_scaling.dts = function(xds_obj, N=25, rbr=1){
  dts_scaling(xds_obj, N, rbr)
}
