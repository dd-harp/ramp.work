## -----------------------------------------------------------------------------
library(ramp.xds)
require(deSolve)
require(rootSolve)
require(ramp.work)

## ----echo=F-------------------------------------------------------------------
#devtools::load_all()

## -----------------------------------------------------------------------------
sis_si <- xde_setup(MYZname = "si")
sis_si <- xde_solve(sis_si)
sis_si <- update_inits(sis_si$outputs$orbits$y_last, sis_si)

## -----------------------------------------------------------------------------
#F_obs
get_stat = function(pars){
  y = pars$outputs$orbits$y_last
  return(F_pr(list_Xvars(y, pars, 1), pars$Xpar[[1]]))
}

get_par = function(pars){
  return(pars$Lpar[[1]]$scale)
}

put_par = function(x, pars){
  pars$Lpar[[1]]$scale = x
  return(pars)
}

## -----------------------------------------------------------------------------
sis_si_fit <- xde_maximize_gof(c(0.8), sis_si, get_stat, get_par, put_par, F_sse, Tmax=100)
get_stat(sis_si_fit)

## -----------------------------------------------------------------------------
sis_si_fit2 <- xde_maximize_gof(c(0.8), sis_si_fit, get_stat, get_par, put_par, F_sse, Tmax=100)
get_stat(sis_si_fit2)
sis_si_fit3 <- xde_maximize_gof(c(0.8), sis_si_fit2, get_stat, get_par, put_par, F_sse, Tmax=100)
get_stat(sis_si_fit3)

