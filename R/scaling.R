
#' @title Scaling for Malaria Metrics
#'
#' @description Compute scaling relationships for malaria metrics
#'
#' @param xds_obj a **`ramp.xds`** model object
#' @param N an integer
#'
#' @note This function dispatches on `xds_obj$forced_by`
#'
#' @return an **`xds`** model object
#'
#' @export
xds_scaling = function(xds_obj, N=30){
  UseMethod("xds_scaling", xds_obj$forced_by)
}

#' @title Compute eir-pr scaling relationships
#'
#' @description This function calls [xds_solve] computes average annual values for the eir, the pr, and other
#' interesting terms and returns a table. It is computed for a xds_obj of class "cohort"
#'
#' @inheritParams xds_scaling
#'
#' @importFrom ramp.xds change_mean_forcing xds_solve
#' @importFrom utils tail
#'
#' @return an **`xds`** model object
#'
#' @export
xds_scaling.eir = function(xds_obj, N=25){
  eir0 <- xds_obj$EIR_obj$eir
  dEIR = 10^seq(-2, 3, length.out=N)/365
  pr = ni = rep(0, N)
  stable_orbits = list()
  times = c(0, 3650, 3650 + 1:365)

  for(i in 1:N){

    xds_obj <- ramp.xds::change_mean_forcing(dEIR[i], xds_obj, 1)
    xds_obj <- ramp.xds::xds_solve(xds_obj, times=times)

    XH <- ramp.xds::get_XH_out(xds_obj, 1)

    stable_orbits[[i]] = list()

    pr_t = tail(XH$true_pr, 365)
    stable_orbits[[i]]$pr = pr_t
    pr[i] = mean(pr_t)

    ni_t = tail(XH$ni, 365)
    stable_orbits[[i]]$ni = ni_t
    ni[i] = mean(ni_t)

    stable_orbits[[i]]$eir = tail(XH$eir, 365)
    stable_orbits[[i]]$aeir = 365*tail(XH$eir, 365)
  }
  xds_obj$scaling <- list(aeir=365*dEIR, eir=dEIR, pr=pr, ni=ni)
  xds_obj$scaling$stable_orbits <- stable_orbits

  xds_obj$EIR_obj$eir <- eir0
  return(xds_obj)
}



#' @title Compute scaling relationships from mosquito emergence through PfPR
#'
#' @description This function calls [xds_solve] to get the scaling
#' relationships for mosquito density over 9 factors of 10, and average annual
#' values for the eir, the pr, and other
#' interesting terms and returns a table.
#'
#' @inheritParams xds_scaling
#'
#' @importFrom utils tail
#' @return **`xds`** xds_obj object
#' @export
xds_scaling.Lambda = function(xds_obj, N=30){

  Lambda0 <- xds_obj$L_obj[[1]]$Lambda
  # get R0
  thresh  <- compute_Lambda_threshold(xds_obj)
  xds_obj <- change_mean_forcing(thresh*1.01, xds_obj)
  xds_obj <- xds_solve(xds_obj, 730, 730)
  xds_obj <- last_to_inits(xds_obj)

  Lambda = c(thresh/100, thresh/5, thresh, thresh*5, thresh*100)

  # setup
  scaling = xds_scaling_Lambda(Lambda[1], xds_obj, list())

  for(i in 2:5)
    scaling = xds_scaling_Lambda(Lambda[i], xds_obj, scaling)

  for(i in 1:N){
    ix = which.max(diff(scaling$pr))
    new_Lambda = with(scaling,(Lambda[ix] + Lambda[ix+1])/2)
    scaling = xds_scaling_Lambda(new_Lambda, xds_obj, scaling)
  }

  xds_obj$scaling <- with(scaling, list(Lambda=Lambda, pr=pr, ni=ni, eir=eir, M=M))
  xds_obj$scaling$aeir = xds_obj$scaling$eir*365
  xds_obj$scaling$stable_orbits <- scaling$orbits

  xds_obj$L_obj[[1]]$Lambda <- Lambda0

  return(xds_obj)
}


#' @title Compute
#'
#' @param Lambda emergence rate
#' @param xds_obj a **`ramp.xds`** xds_obj object
#' @param scaling a list with stored values
#'
#' @return a pair of values
#' @export
xds_scaling_Lambda = function(Lambda, xds_obj, scaling){

  times = c(0, 3650, 3650 + 1:365)

  xds_obj <- ramp.xds::change_mean_forcing(Lambda, xds_obj, 1)
  xds_obj <- ramp.xds::xds_solve(xds_obj, times=times)

  XH <- ramp.xds::get_XH_out(xds_obj, 1)
  MY <- ramp.xds::get_MY_out(xds_obj, 1)

  orbits = list()

  pr_t = tail(XH$true_pr, 365)
  orbits$pr = pr_t
  pr = mean(pr_t)

  ni_t = tail(XH$ni, 365)
  orbits$ni = ni_t
  ni = mean(ni_t)

  eir_t = tail(XH$eir, 365)
  orbits$eir = eir_t
  eir = mean(eir_t)

  M_t = tail(MY$M, 365)
  orbits$M = M_t
  M = mean(M_t)
  N <- length(scaling$orbits)

  if(N == 0){
    scaling = list(Lambda=Lambda, pr=pr, ni=ni, eir=eir, M=M)
    scaling$orbits = list()
    scaling$orbits[[1]] = orbits
  } else {
    scaling$Lambda = c(scaling$Lambda, Lambda)
    ot = order(scaling$Lambda)
    scaling$Lambda = scaling$Lambda[ot]

    scaling$pr = c(scaling$pr, pr)
    scaling$pr = scaling$pr[ot]

    scaling$ni = c(scaling$ni, ni)
    scaling$ni = scaling$ni[ot]

    scaling$eir = c(scaling$eir, eir)
    scaling$eir = scaling$eir[ot]

    scaling$M = c(scaling$M, M)
    scaling$M = scaling$M[ot]

    scaling$orbits[[N+1]] = orbits
    scaling$orbits = scaling$orbits[ot]
  }
  return(scaling)
}

#' @title Get High/Low Values for Lambda
#' @param xds_obj a **`ramp.xds`** xds_obj object
#' @return a pair of values
#' @export
compute_Lambda_threshold = function(xds_obj){
  xds_obj <- xds_solve(xds_obj, 3650, 3650)
  y <- get_last(xds_obj)
  b <- F_infectivity(y,xds_obj,1)
  f <- get_f(xds_obj, 1)
  q <- get_q(xds_obj, 1)
  g <- get_g(xds_obj, 1)
  eip <- xds_obj$MY_obj[[1]]$eip
  VC <- f^2*q^2/g^2*exp(-g*eip)
  H <- get_H(xds_obj)
  D <- HTC(xds_obj, 1)
  R <- b*VC*D/H
  return(1/R)
}

#' @title Using the eirpr matrix and a RM xds_obj, convert pr to Lambda
#' @description This takes a xds_obj and uses the XH component to define
#' the eirpr relationship using `xde_scaling_eir` then calls `xde_scaling_lambda`
#' @param pr a vector
#' @param xds_obj a list that defines an `ramp.xds` xds_obj (*e.g.*,  generated by `xde_setup()`)
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
