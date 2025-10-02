
#' Compute the Index of Seasonal Dispersal for a PR seasonal orbit
#' for the \eqn{i^{th}} element of `eirpr$scaling`
#'
#' @param i the index of the orbit to be evaluated
#' @param pars an **`xds`** object
#'
#' @export
compute_IoSD_pr = function(i, pars){
  pr <- pars$outputs$eirpr$scaling[[i]]$pr
  stats::var(pr)/mean(pr)
}

#' Compute the Index of Seasonal Dispersal for a seasonal pattern
#' \eqn{S(t)} over one year. This assumes that
#' \deqn{\int_0^{365} S(t)dt = 365}
#'
#' If in doubt, use [compute_IoSD_F]
#'
#' @param S a function describing a seasonal pattern
#'
#' @export
compute_IoSD_S = function(S){
  S2 = function(t, mean){(1-S(t))^2}
  return(stats::integrate(S2, 0, 365, mean=mean)$val/365)
}

#' Compute the Index of Dispersal for a function
#' F(t) over one year.
#'
#' @param F a function describing a seasonality function (possibly not normalized)
#'
#' @export
compute_IoSD_F = function(F){
  mean = stats::integrate(F, 0, 365)$val/365

  F2 = function(t, mean){(mean-F(t))^2}
  var = stats::integrate(F2, 0, 365, mean=mean)$val/365
  var/mean^2
}
