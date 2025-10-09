
#' @title Time Since Event
#'
#' @description Compute the time elapsed
#' since the last vector control event
#' for all the spline \eqn{t}-values
#'
#' @param xds_obj a **`ramp.xds`** model object
#'
#' @returns numeric
#' @export
time_since_event = function(xds_obj){
  N = length(xds_obj$data$tt)
  last = rep(0, N)
  for(i in 2:N){
    delta = xds_obj$data$tt[i] - c(xds_obj$events_obj$bednet$jdate, xds_obj$events_obj$irs$jdate)
    last[i] = min(delta[delta>0])
  }
  return(last)
}

#' @title Time Since Event
#'
#' @description Compute the time elapsed
#' since the last vector control event
#' for all the spline \eqn{t}-values
#'
#' @param round_ix the index of the IRS round
#' @param xds_obj a **`ramp.xds`** model object
#' @param N the number of y indices
#'
#' @returns numeric
#' @export
get_yix_after_irs_round = function(round_ix, xds_obj, N=1){
  jdate = xds_obj$events_obj$irs$jdate[round_ix]
  delta = xds_obj$data$tt - jdate
  return(which(delta>0)[1:N])
}

#' @title Time Since Event
#'
#' @description Compute the time elapsed
#' since the last vector control event
#' for all the spline \eqn{t}-values
#'
#' @param round_ix the index of the IRS round
#' @param xds_obj a **`ramp.xds`** model object
#' @param N the number of y indices
#'
#' @returns numeric
#' @export
get_yix_after_bednet_round = function(round_ix, xds_obj, N=1){
  jdate = xds_obj$events_obj$bednet$jdate[round_ix]
  delta = xds_obj$data$tt - jdate
  return(which(delta>0)[1:N])
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
  fitting_get_spline_ty(xds_obj)$yy[trusted_ix] -> trusted_y
  new_y = impute_value(trusted_y, impute_y, length(impute_ix))
  xds_obj <- fitting_change_spline_y(new_y, impute_ix, xds_obj)
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



