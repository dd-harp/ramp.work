
#' Get a set of trusted interpolation points
#'
#' @param ix index or indices of interpolation points
#' @param model a **`ramp.xds`** model object
#' @param trust_ty a text string to dispatch `get_trusted_ty`
#'
#' @returns Interpolation points, \eqn{t,y}, as a list
#' @export
get_trusted_ty = function(ix, model, trust_ty){
  class(trust_ty) <- trust_ty
  UseMethod("get_trusted_ty", trust_ty)
}

#' Get the interpolation points
#' @description Return the \eqn{y} values of the
#'
#' @inheritParams get_trusted_ty
#'
#' @returns Interpolation points, \eqn{t,y}, as a list
#' @export
get_trusted_ty.ix = function(ix, model, trust_ty){
  tt <- model$fitting$data$tt[ix]
  yy <- model$fitting$data$yy[ix]
  return(list(tt=tt, yy=yy, ix=ix))
}

#' Get the interpolation points
#' @description Return the \eqn{y} values of the
#'
#' @inheritParams get_trusted_ty
#'
#'
#' @returns Interpolation points, \eqn{t,y}, as a list
#' @export
get_trusted_ty.unmodified = function(ix, model, trust_ty){
  tix = which(model$fitting$data$modified == 0)
  if(length(tix)==0) tix = which.max(model$fitting$data$yy)
  tt <- model$fitting$data$tt[tix]
  yy <- model$fitting$data$yy[tix]
 return(list(tt=tt, yy=yy))
}


#' Get interpolation points
#' @description Pass a set of values, use the
#' others
#'
#' @inheritParams get_trusted_ty
#'
#'
#' @returns Interpolation points, \eqn{t,y}, as a list
#' @export
get_trusted_ty.nix = function(ix, model, trust_ty){
  tt <- model$fitting$data$tt[-ix]
  yy <- model$fitting$data$yy[-ix]
  return(list(tt=tt, yy=yy))
}

#' Get interpolation points
#' @description Return the first interpolation point
#'
#' @inheritParams get_trusted_ty
#' @importFrom utils head
#'
#' @returns Interpolation points, \eqn{t,y}, as a list
#' @export
get_trusted_ty.first = function(ix, model, trust_ty){
  tt = head(model$fitting$data$tt,1)
  yy = head(model$fitting$data$yy,1)
  return(list(tt=tt, yy=yy))
}

#' Get interpolation points
#'
#' @description The `ix` indicates the interpolation
#' points that were not modified by control. The optional
#' argument `opts$t` should be the time of an event, so
#' the interpolation points are before the event and
#' unmodified by control.
#'
#' @inheritParams get_trusted_ty
#'
#' @returns Interpolation points, \eqn{t,y}, as a list
#' @export
get_trusted_ty.tix = function(ix, model, trust_ty){
  tt <- model$fitting$data$tt[ix]
  tix = 0
  tt <- model$fitting$data$tt[ix[tix]]
  yy <- model$fitting$data$yy[ix[tix]]
  return(list(tt=tt, yy=yy))
}


#' Get interpolation points
#' @description Return the last interpolation point
#'
#' @inheritParams get_trusted_ty
#' @importFrom utils tail
#'
#' @returns Interpolation points, \eqn{t,y}, as a list
#' @export
get_trusted_ty.last= function(ix, model, trust_ty){
  tt = tail(model$fitting$data$tt,1)
  yy = tail(model$fitting$data$yy,1)
  return(list(tt=tt, yy=yy))
}

#' Get interpolation points
#'
#' @description Return all the interpolation points
#'
#' @inheritParams get_trusted_ty
#'
#' @returns Interpolation points, \eqn{t,y}, as a list
#' @export
get_trusted_ty.all = function(ix, model, trust_ty){
  tt = model$fitting$data$tt
  yy = model$fitting$data$yy
  return(list(tt=tt, yy=yy))
}
