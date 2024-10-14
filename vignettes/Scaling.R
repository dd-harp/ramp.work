## -----------------------------------------------------------------------------
library(ramp.xds)
library(ramp.work)
library(deSolve)

## ----echo=F-------------------------------------------------------------------
#devtools::load_all()

## ----Fsin---------------------------------------------------------------------
tt <- seq(0, 730, by=5) 
p1 <- makepar_F_sin(floor=0.1)
Fsin <- make_function(p1)
plot(tt, Fsin(tt), type="l")

## -----------------------------------------------------------------------------
xds_setup_cohort(Xname = "SIS", F_season=Fsin) -> sis

## -----------------------------------------------------------------------------
xds_solve_cohort(sis) -> sis

## -----------------------------------------------------------------------------
xde_scaling_eir(sis, 25) -> sis

## -----------------------------------------------------------------------------
plot_eirpr(sis)

## -----------------------------------------------------------------------------
require(viridis)
clrs = turbo(25)

## ----fig.height=3.5, fig.width=5----------------------------------------------
plot_eirpr(sis)

with(sis$output$eirpr,{
  points(aeir, pr, col = clrs)
  lines(scaling[[5]]$aeir, scaling[[5]]$pr, col = clrs[5])
  lines(scaling[[10]]$aeir, scaling[[10]]$pr, col = clrs[10])
  lines(scaling[[15]]$aeir, scaling[[15]]$pr, col = clrs[15])
  lines(scaling[[20]]$aeir, scaling[[20]]$pr, col = clrs[20])
})

## -----------------------------------------------------------------------------
preir_i = xde_pr2eir(c(0.001, runif(25, 0, 1), 0.999), sis)

## -----------------------------------------------------------------------------
preir_i$errors

## -----------------------------------------------------------------------------
plot_eirpr(sis)
with(sis$outputs$eirpr, points(aeir, pr, pch = 15))
with(preir_i, points(365*eir, pr, pch = 19, col = "red"))

