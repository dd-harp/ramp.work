


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
