
#' Plot EIR(t) *vs.* the PR(t)
#'
#' @param xds_obj a **`ramp.xds`** model object
#' @param i the host species index
#' @param clrs color(s) for plotting
#' @param llty an [integer] that specifies `lty` for plotting
#' @param stable a [logical] set to FALSE for `orbits` and TRUE for `stable_orbits`
#' @param add_axes a [logical] to plot add_axes only if FALSE
#'
#' @export
plot_eirVpr <- function(xds_obj, i=1, clrs="black", llty=1, stable=FALSE, add_axes=TRUE){
  vars=with(xds_obj$outputs,if(stable==TRUE){stable_orbits}else{orbits})

  pr = vars$terms$pr[[i]]
  eir = vars$terms$eir[[i]]
  aeir = 365*eir
  if(add_axes==TRUE){
    plot(aeir, pr, type = "n", xaxt="n", lty = llty,
           xlab = "aEIR", ylab = "PR",
           xlim = range(0, aeir), ylim = c(0,1), col = clrs)
      graphics::axis(1, 10^c(-1:3), c(".1", "1", "10", "100","1000"))
  }
  lines_eirVpr(eir, pr, xds_obj$Hpar[[i]]$nStrata, clrs, llty)
}

#' Add lines for the EIR(t) *vs.* the PR(t)
#'
#' @param eir the daily EIR
#' @param pr the parasite rate
#' @param nStrata the number of population strata
#' @param clrs a [character] vector of colors
#' @param llty an [integer] (or integers) that specifies `lty` for plotting
#'
#' @export
lines_eirVpr <- function(eir, pr, nStrata, clrs= "black", llty = 1){
    aeir = 365*eir
    if(nStrata==1)
      graphics::lines(aeir, pr, col=clrs[1], lty = llty[1])
    if(nStrata>1){
      if(length(clrs)==1) clrs=rep(clrs, nStrata)
      if(length(llty)==1) llty=rep(llty, nStrata)
      for(i in 1:nStrata)
        graphics::lines(aeir[,i], pr[,i], col=clrs[i], lty = llty[i])
  }
}

#' Plot the eir-pr scaling relationship
#'
#' @param xds_obj a **`ramp.xds`** model object
#' @param clrs a [character] vector of colors
#' @param llty a [list]
#'
#' @export
plot_eirpr <- function(xds_obj, clrs= "black", llty = 1){
  with(xds_obj$scaling, {
    ix = which(aeir>0)
    plot(aeir[ix], pr[ix], type = "l", xaxt="n", lty = llty,
         xlab = "aEIR", ylab = "PR", log="x",
         xlim = range(10^-2, 10^3), ylim = c(0,1),
         col = clrs)
    graphics::axis(1, 10^c(-1:3), c(".1", "1", "10", "100","1000"))
  })
  return(invisible())
}

#' Add lines for an eir-pr scaling relationship
#'
#' @param xds_obj a **`ramp.xds`** model object
#' @param clrs a [character] vector of colors
#' @param llty a [list]
#'
#' @export
lines_eirpr <- function(xds_obj, clrs= "black", llty = 1){
  with(xds_obj$scaling, graphics::lines(aeir, pr, col = clrs, lty = llty))
  return(invisible())
}

#' Draw the orbit for the \eqn{i^{th}} element of
#' eirpr$scaling.
#'
#' @param ix the subset of with orbits to plot
#' @param xds_obj a **`ramp.xds`** model object
#' @param clrs a [character] vector of colors
add_eirpr_seasonal_profile = function(ix, xds_obj, clrs){
  with(xds_obj$scaling, points(aeir, pr, col = clrs))
  for(i in ix) add_orbits(i, xds_obj, clrs[i])
}

#' Draw the orbit for the \eqn{i^{th}} element of
#' eirpr$scaling.
#'
#' @param i the index of the orbit to plot
#' @param xds_obj a **`ramp.xds`** model object
#' @param clr a [character] vector of colors
#'
#' @export
add_orbits = function(i, xds_obj, clr){
  with(xds_obj$scaling$stable_orbits[[i]],{
    lines(aeir, pr, col = clr)
    points(mean(aeir), mean(pr), pch=19, col=clr)
  })}

#' Draw the orbit for the \eqn{i^{th}} element of
#' eirpr$scaling, and add points at the
#' minimum and maximum eir and pr
#'
#' @param i the index of the orbit to plot
#' @param xds_obj a **`ramp.xds`** model object
#' @param clr a [character] vector of colors
#'
#' @export
add_orbits_px = function(i, xds_obj, clr){
  add_orbits(i, xds_obj, clr)
  with(xds_obj$output$eirpr$scaling[[i]],{
    ix = which.max(aeir)
    points(aeir[ix], pr[ix], pch=15, col=clr)
    ix = which.min(aeir)
    points(aeir[ix], pr[ix], pch=15, col=clr)
    ix = which.max(pr)
    points(aeir[ix], pr[ix], pch=15, col=clr)
    ix = which.min(pr)
    points(aeir[ix], pr[ix], pch=15, col=clr)
  })}


