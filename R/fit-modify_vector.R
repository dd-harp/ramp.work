
#' Replace
#'
#' @description
#' Replace some values in a V
#' with some values in X
#'
#' @param V the V to modify
#' @param ixV indices to be replaced
#' @param X the new values
#' @param ixX indices to be replaced
#'
#' @returns the modified V
#'
#' @export
modify_vector_X = function(V, ixV, X, ixX){
  UseMethod("modify_vector_X", ixV)
}

#' Replace Values in a List
#'
#' @description If `ix` is NULL, return `X`
#'
#' @inheritParams modify_vector_X
#'
#' @returns the modified V
#'
#' @export
#'
#' @examples
#'
#' modify_vector_X(6:15, c(), 1:10)
#'
modify_vector_X.NULL = function(V, ixV, X, ixX){
  return(X)
}

#' Replace Values in a List
#'
#' @inheritParams modify_vector_X
#'
#' @returns the modified V
#'
#' @export
#'
#' @examples
#'
#' modify_vector_X(6, c(1,7), 1:10)
#'
modify_vector_X.numeric = function(V, ixV, X, ixX){
  V[ixV]=X[ixX]
  return(V)
}

