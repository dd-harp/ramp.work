
#' Get a set of trusted interpolation points
#'
#' @param ix index or indices of interpolation points
#' @param xds_obj a **`ramp.xds`** xds_obj object
#' @param trust_ty a text string to dispatch `get_ty`
#'
#' @returns Interpolation points, \eqn{t,y}, as a list
#' @export
get_ty = function(ix, xds_obj, trust_ty){
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
get_ty.ix = function(ix, xds_obj, trust_ty){
  tt <- xds_obj$data$tt[ix]
  yy <- xds_obj$data$yy[ix]
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
get_ty.unmodified = function(ix, xds_obj, trust_ty){
  tix = which(xds_obj$data$modified == 0)
  if(length(tix)<=2){
    yy <- xds_obj$data$yy*0 + 1
    tt <- xds_obj$data$tt
  } else {
    tt <- xds_obj$data$tt[tix]
    yy <- xds_obj$data$yy[tix]
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
get_ty.nix = function(ix, xds_obj, trust_ty){
  tt <- xds_obj$data$tt[-ix]
  yy <- xds_obj$data$yy[-ix]
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
get_ty.first = function(ix, xds_obj, trust_ty){
  tt = head(xds_obj$data$tt,1)
  yy = head(xds_obj$data$yy,1)
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
get_ty.tix = function(ix, xds_obj, trust_ty){
  tt <- xds_obj$data$tt[ix]
  tix = 0
  tt <- xds_obj$data$tt[ix[tix]]
  yy <- xds_obj$data$yy[ix[tix]]
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
get_ty.last= function(ix, xds_obj, trust_ty){
  tt = tail(xds_obj$data$tt,1)
  yy = tail(xds_obj$data$yy,1)
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
get_ty.all = function(ix, xds_obj, trust_ty){
  tt = xds_obj$data$tt
  yy = xds_obj$data$yy
  return(list(tt=tt, yy=yy))
}
