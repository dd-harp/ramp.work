
#' Compute the Index of Dispersal for a PR orbit
#' for the \eqn{i^{th}} element of
#' eirpr$scaling.
#'
#' @param pars an **`xds`** object
#'
#' @export
compute_IoD_pr = function(i, pars){
  pr <- pars$outputs$eirpr$scaling[[i]]$pr
  var(pr)/mean(pr)
}

#' Compute the Index of Dispersal for a seasonal pattern
#' S(t) over one year. This assumes that
#' \deqn{\int_0^{365} S(t)dt = 365}
#' If in doubt, use [compute_IoD_F]
#'
#' @param pars an **`xds`** object
#' @param clrs a [character] vector of colors
#' @param llty a [list]
#'
#' @export
compute_IoD_S = function(S){
  S2 = function(t, mean){(1-S(t))^2}
  return(integrate(S2, 0, 365, mean=mean)$val/365)
}

#' Compute the Index of Dispersal for a function
#' F(t) over one year.
#'
#' @param pars an **`xds`** object
#' @param clrs a [character] vector of colors
#' @param llty a [list]
#'
#' @export
compute_IoD_F = function(F){
  mean = integrate(F, 0, 365)$val/365

  F2 = function(t, mean){(mean-F(t))^2}
  var = integrate(F2, 0, 365, mean=mean)$val/365
  var/mean^2
}
