
#' @title Convert pr to eir
#' @description Use the scaling table to convert a set of pr values into eir values
#' @param pr a [vector]
#' @param xds_obj a [list]
#' @param extend a [logical] option to determine whether to extend outside the range
#' @export
xde_pr2eir = function(pr, xds_obj, extend=FALSE){
  with(xds_obj$outputs, stopifnot(exists("scaling")))
  PR = xds_obj$outputs$scaling$pr
  EIR = xds_obj$outputs$scaling$eir
  if(extend==TRUE){
    PR = c(0, PR, 1)
    EIR = c(0, EIR, 5*10^3/365)
  }
  ix = which(pr<=max(PR) & pr>=min(PR))
  get_eir = function(pr){
    if(pr == min(PR)) return(min(EIR))
    if(pr == max(PR)) return(max(EIR))
    ix = max(which(PR<pr))
    ff = (pr-PR[ix])/(PR[ix+1]-PR[ix])
    eir=EIR[ix] + ff*(EIR[ix+1]-EIR[ix])
    return(eir)
  }
  eir = 0*pr-1
  eir[ix] = sapply(pr[ix], get_eir)
  pr2eir = list(pr=pr[ix], eir=eir[ix])
  if(length(ix)>0) pr2eir$errors = c(pr=pr[-ix])
  return(pr2eir)
}

#' @title Convert eir to pr
#' @description Use the scaling table to interpolate
#' @param eir a [vector]
#' @param xds_obj a [list]
#' @param extend a [logical] option to determine whether to extend outside the range
#' @export
xde_eir2pr = function(eir, xds_obj, extend=FALSE){
  with(xds_obj$outputs, stopifnot(exists("scaling")))
  PR = xds_obj$outputs$scaling$pr
  EIR = xds_obj$outputs$scaling$eir
  if(extend==TRUE){
    PR = c(0, PR, 1)
    EIR = c(0, EIR, 5*10^3/365)
  }
  ix = which(eir<=max(EIR) & eir>=min(EIR))
  get_pr = function(eir){
    if(eir == min(EIR)) return(min(PR))
    if(eir == max(EIR)) return(max(PR))
    ix = max(which(EIR<eir))
    ff = (eir-EIR[ix])/(EIR[ix+1]-EIR[ix])
    pr=PR[ix] + ff*(PR[ix+1]-PR[ix])
    return(pr)
  }
  pr = 0*eir-1
  pr[ix] = sapply(eir[ix], get_pr)
  eir2pr = list(eir=eir[ix], pr=pr[ix])
  if(length(ix)>0) eir2pr$errors = c(pr=pr[-ix])
  return(eir2pr)
}

#' @title Convert eir to ni
#' @description Use the scaling table to interpolate
#' @param eir a [vector]
#' @param xds_obj a [list]
#' @param extend a [logical] option to determine whether to extend outside the range
#' @export
xde_eir2ni = function(eir, xds_obj, extend=FALSE){
  with(xds_obj$outputs, stopifnot(exists("scaling")))
  NI = xds_obj$outputs$scaling$ni
  EIR = xds_obj$outputs$scaling$eir
  if(extend==TRUE){
    NI = c(0, NI, 1)
    EIR = c(0, EIR, 5*10^3/365)
  }
  ix = which(eir<=max(EIR) & eir>=min(EIR))
  get_ni = function(eir){
    if(eir == min(EIR)) return(min(NI))
    if(eir == max(EIR)) return(max(NI))
    ix = max(which(EIR<eir))
    ff = (eir-EIR[ix])/(EIR[ix+1]-EIR[ix])
    ni=NI[ix] + ff*(NI[ix+1]-NI[ix])
    return(ni)
  }
  ni = 0*eir-1
  ni[ix] = sapply(eir[ix], get_ni)
  eir2ni = list(eir=eir[ix], ni=ni[ix])
  if(length(ix)>0) eir2ni$errors = c(ni=ni[-ix])
  return(eir2ni)
}

#' @title Convert pr to ni
#' @description Use the scaling table to interpolate
#' @param pr a [vector]
#' @param xds_obj a [list]
#' @param extend a [logical] option to determine whether to extend outside the range
#' @export
xde_pr2ni = function(pr, xds_obj, extend=FALSE){
  with(xds_obj$outputs, stopifnot(exists("scaling")))
  PR = xds_obj$outputs$scaling$pr
  NI = xds_obj$outputs$scaling$ni
  if(extend==TRUE){
    PR = c(0, PR, 1)
    NI = c(0, NI, max(NI))
  }
  ix = which(pr<=max(PR) & pr>=min(PR))
  get_ni = function(pr){
    if(pr == min(PR)) return(min(NI))
    if(pr == max(PR)) return(max(NI))
    ix = max(which(PR<pr))
    ff = (pr-PR[ix])/(PR[ix+1]-PR[ix])
    ni=NI[ix] + ff*(NI[ix+1]-NI[ix])
    return(ni)
  }
  ni = 0*pr-1
  ni[ix] = sapply(pr[ix], get_ni)
  pr2ni = list(pr=pr[ix], ni=ni[ix])
  if(length(ix)>0) pr2ni$errors = c(pr=pr[-ix])
  return(pr2ni)
}


#' @title Convert pr to mosquito density
#' @description Use the scaling table to convert a set of pr values into scaled mosquito density values
#' @param pr a [vector]
#' @param xds_obj a [list]
#' @param extend a [logical] option to determine whether to extend outside the range
#' @export
xde_pr2m = function(pr, xds_obj, extend=FALSE){
  with(xds_obj, stopifnot(exists("scaling")))
  PR = xds_obj$scaling$pr
  m = xds_obj$scaling$m
  if(extend==TRUE){
    PR = c(0, PR, 1)
    m = c(0, m, 10*max(m))
  }
  ix = which(pr<=max(PR) & pr>=min(PR))
  get_m = function(pr){
    if(pr == min(PR)) return(min(m))
    if(pr == max(PR)) return(max(m))
    ix = max(which(PR<pr))
    ff = (pr-PR[ix])/(PR[ix+1]-PR[ix])
    m=m[ix] + ff*(m[ix+1]-m[ix])
    return(m)
  }
  mm = 0*pr-1
  mm[ix] = sapply(pr[ix], get_m)
  pr2m = list(pr=pr[ix], m=mm[ix])
  if(length(ix)>0) pr2m$errors = c(pr=pr[-ix])
  return(pr2m)
}

#' @title Convert pr to lambda
#' @description Use the scaling table to convert a set of pr values into lambda values
#' @param pr a [vector]
#' @param xds_obj a [list]
#' @param extend a [logical] option to determine whether to extend outside the range
#' @export
xde_pr2Lambda = function(pr, xds_obj, extend=FALSE){
  with(xds_obj$outputs, stopifnot(exists("scaling")))
  PR = xds_obj$outputs$scaling$pr
  Lambda = xds_obj$outputs$scaling$Lambda
  if(extend==TRUE){
    PR = c(0, PR, 1)
    Lambda = c(0, Lambda, 5*10^3/365)
  }
  ix = which(pr<=max(PR) & pr>=min(PR))
  get_Lambda = function(pr){
    if(pr == min(PR)) return(min(Lambda))
    if(pr == max(PR)) return(max(Lambda))
    ix = max(which(PR<pr))
    ff = (pr-PR[ix])/(PR[ix+1]-PR[ix])
    LLambda=Lambda[ix] + ff*(Lambda[ix+1]-Lambda[ix])
    return(LLambda)
  }
  lLambda = 0*pr-1
  lLambda[ix] = sapply(pr[ix], get_Lambda)
  pr2Lambda = list(pr=pr[ix], Lambda=lLambda[ix])
  if(length(ix)>0) pr2Lambda$errors = c(pr=pr[-ix])
  return(pr2Lambda)
}


#' @title Convert the EIR into a vector describing infective biting density
#' @description Computes a vector of length `nStrata` describing the daily eir, per patch from
#' a vector of length `nPatches` describing the daily infective biting density.
#' \deqn{fqZ = \left(\beta^T \cdot \beta \right)^{-1} \cdot \beta^T \cdot E}
#' @param eir a vector describing the EIR in several strata
#' @param beta the mixing matrix
#' @importFrom MASS ginv
#' @return a numeric [vector]
#' @export
eir2fqZ <- function(eir, beta){
  stopifnot(inherits(beta, 'matrix'))
  stopifnot(is.double(eir))

  BBinv <- ginv(t(beta) %*% beta)
  fqZ <- BBinv %*% t(beta) %*% eir
  return(fqZ)
}

#' @title  Convert a vector describing infective biting density into the EIR, \eqn{E}
#' @description Computes a vector of length `nPatches` describing infective biting density, per patch, from
#' a vector of length `n` describing the daily EIR per stratum:
#' \deqn{E = \beta \cdot fqZ}
#' @param fqZ a vector describing infective biting density
#' @param beta the mixing matrix
#' @importFrom MASS ginv
#' @return a numeric [vector]
#' @export
fqZ2eir <- function(fqZ, beta){
  stopifnot(inherits(beta, 'matrix'))
  stopifnot(is.double(eir))

  eir <- beta %*% fqZ
  return(eir)
}

