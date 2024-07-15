library(devtools)
devtools::install_github("dd-harp/ramp.xde")
devtools::install_github("dd-harp/ramp.work")
devtools::install_github("dd-harp/ramp.library")


## -----------------------------------------------------------------------------
suppressMessages(library(ramp.library))
suppressMessages(library(ramp.xde))
suppressMessages(require(deSolve))
suppressMessages(require(rootSolve))
suppressMessages(require(ramp.work))

## -----------------------------------------------------------------------------
#devtools::load_all()


## -----------------------------------------------------------------------------
sis_si <- xde_setup(MYZname = "si")
sis_si <- xde_solve(sis_si)
sis_si <- update_inits(sis_si$outputs$orbits$y_last, sis_si)

## -----------------------------------------------------------------------------
#F_obs
get_stat = function(pars){
  return(tail(F_pr(pars$outputs$orbits, pars, 1),1))
}

get_par = function(pars){
  return(pars$Lpar[[1]]$scale)
}


put_par = function(x, pars){
  pars$Lpar[[1]]$scale = x
  return(pars)
}

## -----------------------------------------------------------------------------
#devtools::load_all()

## -----------------------------------------------------------------------------
sis_si_fit <- xde_maximize_gof(c(0.8), sis_si, get_stat, get_par, put_par, F_sse, Tmax=100)
get_stat(sis_si_fit)

## -----------------------------------------------------------------------------
sis_si_fit2 <- xde_maximize_gof(c(0.8), sis_si_fit, get_stat, get_par, put_par, F_sse, Tmax=100)
get_stat(sis_si_fit2)
sis_si_fit3 <- xde_maximize_gof(c(0.8), sis_si_fit2, get_stat, get_par, put_par, F_sse, Tmax=100)
get_stat(sis_si_fit3)

