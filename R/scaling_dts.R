


#' @title Scaling for Malaria Metrics
#'
#' @description This function dispatches on `class(xds_obj$forced_by)`
#'
#' @inheritParams xds_scaling
#'
#' @return **`xds`** xds_obj object
#'
#' @export
dts_scaling = function(xds_obj, N=25, rbr=1){
  UseMethod("dts_scaling", xds_obj$forced_by)
}
