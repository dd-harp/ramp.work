## -----------------------------------------------------------------------------
library(ramp.xds)
require(deSolve)
require(rootSolve)
require(ramp.work)

## ----echo=F-------------------------------------------------------------------
devtools::load_all()

## -----------------------------------------------------------------------------
sis_si <- xds_setup(MYZname = "si")
sis_si <- xds_solve(sis_si)

## -----------------------------------------------------------------------------
#F_obs
get_stat = function(pars){
  pr = tail(sis_si$outputs$orbits$XH[[1]]$true_pr, 1)
  return(pr)
}

get_par = function(pars){
  return(pars$Lpar[[1]]$Lambda)
}

put_par = function(x, pars){
  pars$Lpar[[1]]$Lambda = x
  return(pars)
}

