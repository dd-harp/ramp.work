#' Setup Imputation
#'
#' @description This sets up the the imputatation model object. It
#' sets options for using interpolation points to impute a baseline
#' or to hindcast or forecast.
#'
#' @param impute_ty a text string to dispatch `impute_baseline_ty`
#' @param trusted_ty a text string to dispatch `get_trusted_ty`
#'
#' @returns an imputation model object
#'
#'
#' @export
setup_imputation = function(impute_ty = "mean", trusted_ty="unmodified"){

  impute_obj <- list()

  class(impute_ty) = impute_ty
  impute_obj$impute_ty = impute_ty

  class(trusted_ty) = trusted_ty
  impute_obj$trusted_ty = trusted_ty

  return(impute_obj)
}


#' @title Impute the baseline
#'
#' @description Impute the a new \eqn{y} value(s) for one or more
#' interpolation points
#'
#' @param impute_ix the index (or indices) of the \eqn{y} values to replace
#' @param trusted_ix the index (or indices) of a node (or nodes) to replace
#' @param xds_obj a **`ramp.xds`** model object
#' @param impute_y a text string to dispatch `impute_value`
#'
#' @returns an **`ramp.xds`** model object
#'
#' @export
impute_spline_y = function(impute_ix, trusted_ix, xds_obj, impute_y="mean"){
  get_fit_trend(xds_obj)$yy[trusted_ix] -> trusted_y
  new_y = impute_value(trusted_y, impute_y, length(impute_ix))
  xds_obj <- change_ix_fit_spline_y(new_y, impute_ix, xds_obj)
  return(xds_obj)
}


#' @title Impute the baseline
#'
#' @description Impute the \eqn{n} value of one or more
#' interpolation points in the pair, \eqn{t,y}.
#'
#' @param y a set of y values
#' @param impute_y a text string to dispatch `impute_value`
#' @param N the length of the return value
#'
#' @returns numeric
#' @export
impute_value = function(y, impute_y, N=1){
  class(impute_y) <- impute_y
  UseMethod("impute_value", impute_y)
}

#' @title Use Mean for Modify Baseline
#' @description
#' This function modifies
#'
#' @inheritParams impute_value
#'
#' @returns numeric
#' @export
impute_value.asis = function(y, impute_y, N=1){
  y
}

#' @title Use Mean for Modify Baseline
#' @description
#' This function modifies
#'
#' @inheritParams impute_value
#'
#' @returns numeric
#' @export
impute_value.reverse = function(y, impute_y, N=1){
  rev(y)
}

#' @title Use Mean for Modify Baseline
#' @description
#' This function modifies
#'
#' @inheritParams impute_value
#'
#' @returns numeric
#' @export
impute_value.first = function(y, impute_y, N=1){
  rep(head(y, 1), N)
}

#' @title Use Mean for Modify Baseline
#' @description
#' This function modifies
#'
#' @inheritParams impute_value
#'
#' @returns numeric
#' @export
impute_value.last = function(y, impute_y, N=1){
  rep(tail(y, 1), N)
}

#' @title Use Mean for Modify Baseline
#' @description
#' This function modifies
#'
#' @inheritParams impute_value
#'
#' @returns numeric
#' @export
impute_value.mean = function(y, impute_y, N=1){
  rep(mean(y), N)
}

#' @title Use Max for Modify Baseline
#' @description
#' This function modifies
#'
#' @inheritParams impute_value
#'
#' @returns numeric
#' @export
impute_value.max = function(y, impute_y, N=1){
  rep(max(y), N)
}

#' @title Use Min for Modify Baseline
#' @description
#' This function modifies
#'
#' @inheritParams impute_value
#'
#' @returns numeric
#' @export
impute_value.min = function(y, impute_y, N=1){
  rep(min(y), N)
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
impute_value.median = function(y, impute_y, N=1){
  rep(median(y), N)
}

#' @title Baseline with gamma predictions
#' @description
#' This function modifies
#'
#' @inheritParams impute_value
#'
#' @returns numeric
#' @export
impute_value.gam = function(y, impute_y, N=1){
  gam_forecast(y, N)
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
impute_value.subsamp = function(y, impute_y, N=1){
  sample(y, N, replace=T)
}



