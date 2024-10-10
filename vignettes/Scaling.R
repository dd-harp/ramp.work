## -----------------------------------------------------------------------------
library(ramp.xds)
library(deSolve)
library(rootSolve)
library(ramp.work)

## ----echo=F-------------------------------------------------------------------
devtools::load_all()

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

## -----------------------------------------------------------------------------
sis0 <- xds_setup_cohort(Xname = "SIS", F_season = Fsin)
xde_scaling_eir(sis0, 25) -> sis0

## -----------------------------------------------------------------------------
clrs = turbo(25)
with(sis$outputs$eirpr, plot(aeir, pr, type = "l", log = "x", xaxt= "n", xlab = "aEIR", ylab = "PR"))
axis(1, 10^(-1:3), c(0.1, 1, 10, 100, 1000))
lines(sis$outputs$eirpr$aeir, sis0$outputs$eirpr$pr, col = "tomato", lwd=2) 

with(sis0$outputs$eirpr, points(aeir, pr, col = clrs))
with(sis0$outputs$eirpr, lines(scaling[[5]]$aeir, scaling[[5]]$pr, col = clrs[5]))
with(sis0$outputs$eirpr, lines(scaling[[10]]$aeir, scaling[[10]]$pr, col = clrs[10]))
with(sis0$outputs$eirpr, lines(scaling[[15]]$aeir, scaling[[15]]$pr, col = clrs[15]))
with(sis0$outputs$eirpr, lines(scaling[[20]]$aeir, scaling[[20]]$pr, col = clrs[20]))

## ----eval=F-------------------------------------------------------------------
#  sip = xds_setup_cohort(Xname = "SIP", F_season=Fsin)
#  sip$Xpar[[1]]$eta = 1/40
#  xde_scaling_eir(sip, 25) -> sip

## ----eval=F-------------------------------------------------------------------
#  sip1 = setup_exposure_nb(sip, 1/50)
#  xde_scaling_eir(sip1, 25) -> sip1

## ----eval=F-------------------------------------------------------------------
#  with(sis$outputs$eirpr, plot(aeir, pr, type = "l", log = "x", xaxt= "n", xlab = "aEIR", ylab = "PR"))
#  axis(1, 10^(-1:3), c(0.1, 1, 10, 100, 1000))
#  with(sip$outputs$eirpr, lines(aeir, pr, col = "darkorange"))
#  with(sip1$outputs$eirpr, lines(aeir, pr, col = "brown"))

## -----------------------------------------------------------------------------
sis4 <- setup_exposure_nb(sis, 1/50)
xde_scaling_eir(sis4, 25) -> sis4

## -----------------------------------------------------------------------------
with(sis$outputs$eirpr, plot(aeir, pr, type = "l", log = "x", xaxt= "n", xlab = "aEIR", ylab = "PR"))
axis(1, 10^(-1:3), c(0.1, 1, 10, 100, 1000))
#with(sis2$outputs$eir, lines(aeir, pr, col = "blue"))
#with(sis3$outputs$eir, lines(aeir, pr, col = "purple"))
with(sis4$outputs$eir, lines(aeir, pr, col = "darkblue"))

## -----------------------------------------------------------------------------
sis5 <- setup_travel_static(sis, delta = 1/5/365)
xde_scaling_eir(sis5, 25) -> sis5

## -----------------------------------------------------------------------------
with(sis$outputs$eirpr, plot(aeir, pr, type = "l", log = "x", xaxt= "n", xlab = "aEIR", ylab = "PR"))
axis(1, 10^(-1:3), c(0.1, 1, 10, 100, 1000))
with(sis5$outputs$eir, lines(aeir, pr, col = "darkgreen")) 

