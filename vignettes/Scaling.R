## -----------------------------------------------------------------------------
library(ramp.xds)
library(ramp.work)
library(deSolve)
library(viridisLite)

## ----echo=F-------------------------------------------------------------------
#devtools::load_all()

## ----Fsin, fig.height=4, fig.width=7------------------------------------------
tt <- seq(0, 730, by=5) 
p1 <- makepar_F_sin(bottom=0.2, pw=1.2)
Fsin <- make_function(p1)
plot(tt, Fsin(tt), type="l", xlab = "Time (Days)", ylab = "Seasonal Pattern")

## -----------------------------------------------------------------------------
xds_setup_cohort(Xname = "SIS", F_season=Fsin) -> sis

## -----------------------------------------------------------------------------
xds_solve_cohort(sis) -> sis

## -----------------------------------------------------------------------------
xde_scaling_eir(sis, 25) -> sis

## ----fig.height=4, fig.width=7------------------------------------------------
plot_eirpr(sis)

## ----fig.height=4, fig.width=7, eval=F----------------------------------------
#  eirpr_seasonal_profile(c(5,10,15,20), sis, clrs = turbo(25))

## ----echo=F-------------------------------------------------------------------
#devtools::load_all()

## -----------------------------------------------------------------------------
Lo = list(
  F_season = Fsin
)
xds_setup(Xname = "SIS", Lopts=Lo) -> sis_full

## -----------------------------------------------------------------------------
xds_solve(sis_full) -> sis_full

## -----------------------------------------------------------------------------
xde_scaling_lambda(sis_full, 1.1, 30) -> sis_full

## -----------------------------------------------------------------------------
plot_eirpr(sis_full)

## -----------------------------------------------------------------------------
with(sis_full$outputs$eirpr, plot(Ro, pr, type = "l", log="x"))

## -----------------------------------------------------------------------------
with(sis_full$outputs$eirpr, plot(M, pr, type = "l", log = "x"))
with(sis_full$outputs$eirpr$scaling[[18]], lines(M, pr, type = "l"))

## -----------------------------------------------------------------------------
preir_i = xde_pr2eir(c(0.001, runif(25, 0, 1), 0.999), sis)

## -----------------------------------------------------------------------------
preir_i$errors

## -----------------------------------------------------------------------------
plot_eirpr(sis)
with(sis$outputs$eirpr, points(aeir, pr, pch = 15))
with(preir_i, points(365*eir, pr, pch = 19, col = "red"))

