#' @title `sigX`
#'
#' @description Ensure the pw
#' parameter is properly bounded
#'
#' @param x a number
#' @param mn the asymptotic value at negative infinity
#' @param mx the asymptotic value at infinity
#'
#' @returns a number
#' @export
sigX = function(x, mn=0, mx=1){
  mn + (mx-mn)*exp(x)/(1+exp(x))
}

#' @title `sigX`
#'
#' @description Ensure the pw
#' parameter is properly bounded
#'
#' @param x a number
#' @param mn the asymptotic value at negative infinity
#' @param mx the asymptotic value at infinity
#'
#' @returns a number
#' @export
sigXinv = function(x, mn=0, mx=1){
  stopifnot(x<mx)
  stopifnot(x>mn)
  log((x-mn)/(mx-x))
}
