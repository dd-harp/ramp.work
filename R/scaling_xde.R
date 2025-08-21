


#' @title Scaling for Malaria Metrics
#'
#' @description This function dispatches on `class(xds_obj$forced_by)`
#'
#' @inheritParams xds_scaling
#'
#' @return a **`ramp.xds`** model object
#'
#' @export
xde_scaling = function(xds_obj, N=25, rbr=1){
  UseMethod("xde_scaling", xds_obj$forced_by)
}

#' @title Compute eir-pr scaling relationships
#' @description This function calls [xds_solve_cohort] computes average annual values for the eir, the pr, and other
#' interesting terms and returns a table. It is computed for a xds_obj of class "cohort"
#' @param xds_obj a **`ramp.xds`** model object
#' @param N an integer
#' @param rbr the relative biting rate for a population stratum
#' @importFrom utils tail
#' @return a **`ramp.xds`** model object
#' @export
xde_scaling.eir = function(xds_obj, N=25, rbr=1){

  aEIR = 10^seq(-1, 3, length.out=N)
  pr = ni = eir = rep(0, N)
  stable_orbits = list()
  for(i in 1:N){
    xds_obj$EIRpar$eir <- rbr*aEIR[i]/365
    xds_obj <- ramp.xds::xds_solve(xds_obj, 3650, 1)
    XH <- ramp.xds::get_XH(xds_obj, 1)
    pr_t = tail(XH$true_pr, 365); pr[i] = mean(pr_t)
    ni_t = tail(XH$ni, 365);  ni[i]= mean(ni_t)
    eir_t = tail(XH$eir, 365)*1/rbr; eir[i] = mean(eir_t)
    stable_orbits[[i]] = list(aeir = eir_t*365, eir = eir_t, pr = pr_t, ni = ni_t)
  }

  xds_obj$scaling <- list(aeir=365*eir, eir=eir, pr=pr, ni=ni, stable_orbits=stable_orbits)

  return(xds_obj)
}

#' @title Compute scaling relationships from mosquito emergence through PfPR
#' @description This function calls [xds_solve] to get the scaling
#' relationships for mosquito density over 9 factors of 10, and average annual
#' values for the eir, the pr, and other
#' interesting terms and returns a table.
#' @param xds_obj a **`ramp.xds`** xds_obj object
#' @param N an integer
#' @param rbr the relative biting rate for a population stratum
#' @importFrom utils tail
#' @return a **`ramp.xds`** model object
#' @export
xde_scaling.Lambda = function(xds_obj,  N=30, rbr=1){

  # get R0
  thresh <- compute_threshold(xds_obj)
  factor = c(1/100,3/100,1/10,1/5,1/2,2/3,4/5, 9/10, .95, .98, 1, 1.01, 1.02, 1.04, 1.08, 1.12, 1.2, 1.5, 2, 4, 8, 16, 32, 64, 128)
  #ff = function(n){(1.01 + 0.02*n)^n}
  #vv =  ff(1:16)
  #factor = c(1/ff(seq(12, 2, by=-2)), 1, vv)
  N = length(factor)

  # setup
  M = rep(0, N)
  pr = rep(0, N)
  ni = rep(0, N)
  eir = rep(0, N)
  stable_orbits = list()

  xds_obj0=xds_obj
  for(i in 1:N){
    xds_obj <- xds_obj0
    # Run it for awhile
    xds_obj$Lpar[[1]]$Lambda <- thresh*factor[i]
    xds_obj <- ramp.xds::xds_solve(xds_obj, Tmax=7300, dt=7300)
    xds_obj <- last_to_inits(xds_obj)
    xds_obj <- ramp.xds::xds_solve(xds_obj, Tmax=3*365, dt=1)
    average_PR_true(xds_obj) -> pr_t; pr_t <- tail(pr_t, 365)
    pr[i] <- mean(pr_t)
    average_EIR(xds_obj) -> eir_t; eir_t <- tail(eir_t, 365)
    eir[i] <- mean(eir_t)
    MYZ <- get_MYZ(xds_obj)
    if(xds_obj$nPatches==1) M_t <- MYZ$M
    if(xds_obj$nPatches>1) M_t <- colSums(MYZ$M);
    M_t <- tail(M_t, 365)
    M[i] <- mean(M_t)
    XH <- get_XH(xds_obj)
    ni_t = tail(XH$ni, 365);  ni[i]= mean(ni_t)
    stable_orbits[[i]] = list(Lambda = thresh*factor[i], aeir = eir_t*365, M = M_t, eir = eir_t, pr = pr_t, ni = ni_t)
  }
  Lambda <- thresh*factor
  lambda <- Lambda/get_H(xds_obj)
  xds_obj$scaling <- list(Ro=factor, Lambda = Lambda, lambda=lambda, aeir=365*eir, eir=eir, M=M, pr=pr, ni=ni, stable_orbits=stable_orbits)

  return(xds_obj)
}


#' @title Get High/Low Values for Lambda
#' @param xds_obj a **`ramp.xds`** xds_obj object
#' @return a pair of values
#' @export
compute_threshold = function(xds_obj){
  xds_obj <- xds_solve(xds_obj, 3650, 3650)
  y <- get_last(xds_obj)
  b <- F_b(y,xds_obj,1)
  f <- get_f(xds_obj, 1)
  q <- get_q(xds_obj, 1)
  g <- get_g(xds_obj, 1)
  eip <- xds_obj$MYZpar[[1]]$eip
  VC <- f^2*q^2/g^2*exp(-g*eip)
  H <- get_H(xds_obj)
  D <- HTC(xds_obj, 1)
  R <- b*VC*D/H
  return(1/R)
}

#' @title Set up the MYZss object for `xds_scaling_lambda`
#' @description This function computes several quantities that are require
#' @param xds_obj a **`ramp.xds`** model object
#' @export
ssMYZ = function(xds_obj){with(xds_obj$MYZpar,{
  MYZss = list()
  Omega = make_Omega_xde(g, sigma, mu, calK)
  MYZss$Omega = Omega
  MYZss$OmegaInv = solve(Omega)
  MYZss$Upsilon = expm(-Omega*eip)
  MYZss$UpsilonInv = expm(Omega*eip)
  beta = with(xds_obj$Hpar[[1]], compute_beta(H, wts_f, TaR))
  MYZss$beta = beta
  MYZss$betaInv = solve(beta)
  MYZss$f = f
  MYZss$q = q
  MYZss$g = g
  MYZss$eip = eip
  xds_obj$MYZss = MYZss
  return(xds_obj)
})}

#' @title Using the eirpr matrix and a RM xds_obj, convert pr to Lambda
#' @description This takes a xds_obj and uses the XH component to define
#' the eirpr relationship using `xds_scaling` then calls `xds_scaling`
#' @param pr a vector
#' @param xds_obj a **`ramp.xds`** model object
#' @param constrain a logical, if TRUE then set all negative values to zero
#' @export
pr2Lambda = function(pr, xds_obj, constrain=TRUE){
  with(xds_obj, stopifnot(exists("MYZss")))
  with(xds_obj$MYZss,{
    eir = xde_pr2eir(pr, xds_obj, TRUE)$eir
    kappa = xde_pr2ni(pr, xds_obj, TRUE)$ni
    Z = (betaInv %*% eir)/f/q
    Y = OmegaInv %*% (UpsilonInv %*% (Omega %*% Z))
    M = diag(1/f/q/kappa, xds_obj$nPatches)%*%(diag(f*q*kappa, xds_obj$nPatches) + Omega) %*% Y
    Lambda = Omega %*% M
    if(constrain == TRUE) Lambda = pmax(Lambda,0)
    return(Lambda)
  })
}
