
#' @title Impute the baseline
#'
#' @description Impute the \eqn{n} value of one or more
#' interpolation points in the pair, \eqn{t,y}.
#'
#' @param ix the index (or indices) of a node (or nodes) to replace
#' @param xds_obj a **`ramp.xds`** model object
#' @param impute_ty a text string to dispatch `impute_baseline_ty`
#' @param trust_ty a text string to dispatch `get_ty`
#'
#' @returns numeric
#' @export
impute_baseline_ty = function(ix, xds_obj, impute_ty, trust_ty){
  value <- impute_value(ix, xds_obj, impute_ty, trust_ty)
  xds_obj$data$yy[ix] = value
  xds_obj <- update_fitting_ty(xds_obj)
  xds_obj <- setup_trend_par(xds_obj)
  xds_obj <- burnin(xds_obj)
  return(xds_obj)
}

#' @title Impute the baseline
#'
#' @description Impute the \eqn{n} value of one or more
#' interpolation points in the pair, \eqn{t,y}.
#'
#' @param ix the index (or indices) of a node (or nodes) to replace
#' @param xds_obj a **`ramp.xds`** model object
#' @param impute_ty a text string to dispatch `impute_value`
#' @param trust_ty a text string to dispatch `get_ty`
#' @param N (optional) the number to return
#'
#' @returns numeric
#' @export
impute_value = function(ix, xds_obj, impute_ty, trust_ty, N=c()){
  class(impute_ty) <- impute_ty
  UseMethod("impute_value", impute_ty)
}

#' @title Use Mean for Modify Baseline
#' @description
#' This function modifies
#'
#' @inheritParams impute_value
#'
#' @returns numeric
#' @export
impute_value.all = function(ix, xds_obj, impute_ty, trust_ty, N=c()){
  get_ty(ix, xds_obj, trust_ty)$yy
}

#' @title Use Mean for Modify Baseline
#' @description
#' This function modifies
#'
#' @inheritParams impute_value
#'
#' @returns numeric
#' @export
impute_value.asis = function(ix, xds_obj, impute_ty, trust_ty, N=c()){
  get_ty(ix, xds_obj, trust_ty)$yy
}

#' @title Use Mean for Modify Baseline
#' @description
#' This function modifies
#'
#' @inheritParams impute_value
#'
#' @returns numeric
#' @export
impute_value.reverse = function(ix, xds_obj, impute_ty, trust_ty, N=c()){
  rev(get_ty(ix, xds_obj, trust_ty)$yy)
}

#' @title Use Mean for Modify Baseline
#' @description
#' This function modifies
#'
#' @inheritParams impute_value
#'
#' @returns numeric
#' @export
impute_value.first = function(ix, xds_obj, impute_ty, trust_ty, N=c()){
  head(get_ty(ix, xds_obj, trust_ty)$yy, 1)
}

#' @title Use Mean for Modify Baseline
#' @description
#' This function modifies
#'
#' @inheritParams impute_value
#'
#' @returns numeric
#' @export
impute_value.last = function(ix, xds_obj, impute_ty, trust_ty, N=c()){
  tail(get_ty(ix, xds_obj, trust_ty)$yy, 1)
}

#' @title Use Mean for Modify Baseline
#' @description
#' This function modifies
#'
#' @inheritParams impute_value
#'
#' @returns numeric
#' @export
impute_value.mean = function(ix, xds_obj, impute_ty, trust_ty, N=c()){
  ty <- get_ty(ix, xds_obj, trust_ty)
  mean(ty$yy)
}

#' @title Use Max for Modify Baseline
#' @description
#' This function modifies
#'
#' @inheritParams impute_value
#'
#' @returns numeric
#' @export
impute_value.max = function(ix, xds_obj, impute_ty, trust_ty, N=c()){
  ty <- get_ty(ix, xds_obj, trust_ty)
  max(ty$yy)
}

#' @title Use Min for Modify Baseline
#' @description
#' This function modifies
#'
#' @inheritParams impute_value
#'
#' @returns numeric
#' @export
impute_value.min = function(ix, xds_obj, impute_ty, trust_ty, N=c()){
  ty <- get_ty(ix, xds_obj, trust_ty)
  min(ty$yy)
}

#' @title Use Median for Modify Baseline
#' @description
#' This function modifies
#'
#' @inheritParams impute_value
#'
#' @returns numeric
#' @importFrom stats median
#' @export
impute_value.median = function(ix, xds_obj, impute_ty, trust_ty, N=c()){
  ty <- get_ty(ix, xds_obj, trust_ty)
  median(ty$yy)
}

#' @title Baseline with gamma predictions
#' @description
#' This function modifies
#'
#' @inheritParams impute_value
#'
#' @returns numeric
#' @export
impute_value.gam = function(ix, xds_obj, impute_ty, trust_ty, N=c()){
  if(length(N)==0) N=length(ix)
  ty <- get_ty(ix, xds_obj, trust_ty)
  gam_forecast(ty$yy, N)
}

#' Fit and draw
#'
#' @param y values to fit
#' @param N the number of observations
#'
#' @importFrom stats rgamma dgamma optimize
#' @returns random draws
#' @export
gam_forecast = function(y, N){
  gam_lik <- function(shp, yy){
    -sum(log(dgamma(y, shp, shp)))
  }
  optimize(gam_lik, c(0, 10), y)$min -> shape
  rgamma(N, shape, shape)
}

#' @title Create a modify Baseline
#' @description
#' This function modifies
#'
#' @inheritParams impute_value
#'
#' @returns numeric
#' @export
impute_value.subsamp = function(ix, xds_obj, impute_ty, trust_ty, N=c()){
  if(length(N)==0) N=length(ix)
  ty <- get_ty(ix, xds_obj, trust_ty)
  sample(ty$yy, N, replace=T)
}



