## ----echo=F-------------------------------------------------------------------
library(ramp.xds)
#library(ramp.work)
devtools::load_all()

## ----read---------------------------------------------------------------------
dt <- read.csv("pseudo.csv", header=T)
with(dt, {
  plot(tt/365, obs_pr, xlab = "Time (in years)", ylab = "PR")
  lines(tt/365, pr)  
})

## ----shell--------------------------------------------------------------------
mod  <- xds_setup_cohort(season_par = makepar_F_sin())

## ----pr2eir_history-----------------------------------------------------------
mod1 <- with(dt, pr2eir_history(obs_pr, tt, mod, twice=FALSE))

## -----------------------------------------------------------------------------
Fx <- mod1$EIRpar$F_trend
with(dt, integrate(Fx,min(tt), max(tt))$val/(max(tt)-min(tt))) 

## -----------------------------------------------------------------------------
mod2 <- with(dt, pr2eir_history(obs_pr, tt, mod, twice=TRUE))

## ----vis----------------------------------------------------------------------
ttn = seq(-365, max(dt$tt), length.out=600)
xds_plot_PR(mod1)
xds_plot_PR(mod2, clrs="blue", add=TRUE)
with(dt, {
  lines(tt, obs_pr, xlab = "Time (in years)", ylab = "PR")
  lines(tt, pr, col = "red")  
})


