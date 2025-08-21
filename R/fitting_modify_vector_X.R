
#' Modify some values in a vector
#'
#' @description
#' Replace some values in a vector
#' and return the modified vector
#'
#' @param X the new values
#' @param ix indices to be replaced
#' @param vector the vector to modify
#'
#' @returns the modified vector
#'
#' @export
modify_vector_X = function(X, ix, vector){
  UseMethod("modify_vector_X", ix)
}

#' Replace Values in a List
#'
#' @description If `ix` is NULL, return `X`
#'
#'
#' @param X the new values
#' @param ix indices to be replaced
#' @param vector the vector to modify
#'
#' @returns the modified vector
#'
#' @export
#'
#' @examples
#'
#' modify_vector_X(6:15, c(), 1:10)
#'
modify_vector_X.NULL = function(X, ix, vector){
  stopifnot(length(X)==length(vector))
  return(X)
}

#' Replace Values in a List
#'
#' @description If `ix` is numeric
#'
#'
#' @param X the new values
#' @param ix indices to be replaced
#' @param vector the vector to modify
#'
#' @returns the modified vector
#'
#' @export
#'
#' @examples
#'
#' modify_vector_X(6, c(1,7), 1:10)
#'
modify_vector_X.numeric = function(X, ix, vector){
  vector[ix]=X
  return(vector)
}

#' Replace Values in a List
#'
#' @description If `ix` is a list,
#' replace the values in the indices in the
#' \eqn{i^{th}} element of the list
#' with the \eqn{i^{th}} element of `X`
#'
#' @param X the new values
#' @param ix indices to be replaced
#' @param vector the vector to modify
#'
#' @returns the modified vector
#'
#' @export
#'
#' @examples
#' a = list()
#' a[[1]] = c(1,7)
#' a[[2]] = c(3,9)
#' modify_vector_X(c(6,4), a, 1:10)
#'
modify_vector_X.list = function(X, ix, vector){
  stopifnot(length(X)==length(ix))
  for(i in 1:length(ix)){
    vector[ix[[i]]]=X[i]
  }
  return(vector)
}
