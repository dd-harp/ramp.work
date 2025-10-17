
#' Get a set of trusted interpolation points
#'
#' @param ix index or indices of interpolation points
#' @param xds_obj a **`ramp.xds`** xds_obj object
#' @param trust_ty a text string to dispatch `get_trend_ty`
#'
#' @returns Interpolation points, \eqn{t,y}, as a list
#' @export
get_trend_ty = function(ix, xds_obj, trust_ty){
  class(trust_ty) <- trust_ty
  UseMethod("get_trend_ty", trust_ty)
}

#' Get the interpolation points
#' @description Return the \eqn{y} values of the
#'
#' @inheritParams get_trend_ty
#'
#' @returns Interpolation points, \eqn{t,y}, as a list
#' @export
get_trend_ty.ix = function(ix, xds_obj, trust_ty){
  tt <- xds_obj$data_obj$tt[ix]
  yy <- xds_obj$data_obj$yy[ix]
  return(list(tt=tt, yy=yy, ix=ix))
}

#' Get the interpolation points
#' @description Return the \eqn{y} values of the
#'
#' @inheritParams get_trend_ty
#'
#'
#' @returns Interpolation points, \eqn{t,y}, as a list
#' @export
get_trend_ty.unmodified = function(ix, xds_obj, trust_ty){
  tix = which(xds_obj$data_obj$modified == 0)
  if(length(tix)<=2){
    yy <- xds_obj$data_obj$yy*0 + 1
    tt <- xds_obj$data_obj$tt
  } else {
    tt <- xds_obj$data_obj$tt[tix]
    yy <- xds_obj$data_obj$yy[tix]
  }
 return(list(tt=tt, yy=yy))
}


#' Get interpolation points
#' @description Pass a set of values, use the
#' others
#'
#' @inheritParams get_trend_ty
#'
#'
#' @returns Interpolation points, \eqn{t,y}, as a list
#' @export
get_trend_ty.nix = function(ix, xds_obj, trust_ty){
  tt <- xds_obj$data_obj$tt[-ix]
  yy <- xds_obj$data_obj$yy[-ix]
  return(list(tt=tt, yy=yy))
}

#' Get interpolation points
#' @description Return the first interpolation point
#'
#' @inheritParams get_trend_ty
#' @importFrom utils head
#'
#' @returns Interpolation points, \eqn{t,y}, as a list
#' @export
get_trend_ty.first = function(ix, xds_obj, trust_ty){
  tt = head(xds_obj$data_obj$tt,1)
  yy = head(xds_obj$data_obj$yy,1)
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
#' @inheritParams get_trend_ty
#'
#' @returns Interpolation points, \eqn{t,y}, as a list
#' @export
get_trend_ty.tix = function(ix, xds_obj, trust_ty){
  tt <- xds_obj$data_obj$tt[ix]
  tix = 0
  tt <- xds_obj$data_obj$tt[ix[tix]]
  yy <- xds_obj$data_obj$yy[ix[tix]]
  return(list(tt=tt, yy=yy))
}


#' Get interpolation points
#' @description Return the last interpolation point
#'
#' @inheritParams get_trend_ty
#' @importFrom utils tail
#'
#' @returns Interpolation points, \eqn{t,y}, as a list
#' @export
get_trend_ty.last= function(ix, xds_obj, trust_ty){
  tt = tail(xds_obj$data_obj$tt,1)
  yy = tail(xds_obj$data_obj$yy,1)
  return(list(tt=tt, yy=yy))
}

#' Get interpolation points
#'
#' @description Return all the interpolation points
#'
#' @inheritParams get_trend_ty
#'
#' @returns Interpolation points, \eqn{t,y}, as a list
#' @export
get_trend_ty.all = function(ix, xds_obj, trust_ty){
  tt = xds_obj$data_obj$tt
  yy = xds_obj$data_obj$yy
  return(list(tt=tt, yy=yy))
}
