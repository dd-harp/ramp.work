
#' Get a set of trusted interpolation points
#'
#' @param ix index or indices of interpolation points
#' @param model a **`ramp.xds`** model object
#' @param trust_ty a text string to dispatch `get_ty`
#'
#' @returns Interpolation points, \eqn{t,y}, as a list
#' @export
get_ty = function(ix, model, trust_ty){
  class(trust_ty) <- trust_ty
  UseMethod("get_ty", trust_ty)
}

#' Get the interpolation points
#' @description Return the \eqn{y} values of the
#'
#' @inheritParams get_ty
#'
#' @returns Interpolation points, \eqn{t,y}, as a list
#' @export
get_ty.ix = function(ix, model, trust_ty){
  tt <- model$data$tt[ix]
  yy <- model$data$yy[ix]
  return(list(tt=tt, yy=yy, ix=ix))
}

#' Get the interpolation points
#' @description Return the \eqn{y} values of the
#'
#' @inheritParams get_ty
#'
#'
#' @returns Interpolation points, \eqn{t,y}, as a list
#' @export
get_ty.unmodified = function(ix, model, trust_ty){
  tix = which(model$data$modified == 0)
  if(length(tix)<=2){
    yy <- model$data$yy*0 + 1
    tt <- model$data$tt
  } else {
    tt <- model$data$tt[tix]
    yy <- model$data$yy[tix]
  }
 return(list(tt=tt, yy=yy))
}


#' Get interpolation points
#' @description Pass a set of values, use the
#' others
#'
#' @inheritParams get_ty
#'
#'
#' @returns Interpolation points, \eqn{t,y}, as a list
#' @export
get_ty.nix = function(ix, model, trust_ty){
  tt <- model$data$tt[-ix]
  yy <- model$data$yy[-ix]
  return(list(tt=tt, yy=yy))
}

#' Get interpolation points
#' @description Return the first interpolation point
#'
#' @inheritParams get_ty
#' @importFrom utils head
#'
#' @returns Interpolation points, \eqn{t,y}, as a list
#' @export
get_ty.first = function(ix, model, trust_ty){
  tt = head(model$data$tt,1)
  yy = head(model$data$yy,1)
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
#' @inheritParams get_ty
#'
#' @returns Interpolation points, \eqn{t,y}, as a list
#' @export
get_ty.tix = function(ix, model, trust_ty){
  tt <- model$data$tt[ix]
  tix = 0
  tt <- model$data$tt[ix[tix]]
  yy <- model$data$yy[ix[tix]]
  return(list(tt=tt, yy=yy))
}


#' Get interpolation points
#' @description Return the last interpolation point
#'
#' @inheritParams get_ty
#' @importFrom utils tail
#'
#' @returns Interpolation points, \eqn{t,y}, as a list
#' @export
get_ty.last= function(ix, model, trust_ty){
  tt = tail(model$data$tt,1)
  yy = tail(model$data$yy,1)
  return(list(tt=tt, yy=yy))
}

#' Get interpolation points
#'
#' @description Return all the interpolation points
#'
#' @inheritParams get_ty
#'
#' @returns Interpolation points, \eqn{t,y}, as a list
#' @export
get_ty.all = function(ix, model, trust_ty){
  tt = model$data$tt
  yy = model$data$yy
  return(list(tt=tt, yy=yy))
}
