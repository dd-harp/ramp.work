## ----echo=F-------------------------------------------------------------------
library(ramp.xds)
library(ramp.work)

## ----echo=F-------------------------------------------------------------------
#devtools::load_all()

## -----------------------------------------------------------------------------
set.seed(127)
F_s1 <- make_function(makepar_F_sin(bottom=1))
F_t1 <- make_function(makepar_F_spline(365*(0:5), c(.7, 2, 1.5, .9, .6, .8)))
tt <- seq(0, 365*5, by = 30)
pr <- .28*F_s1(tt)*F_t1(tt)
obs_pr <- rnorm(length(tt), pr, .1) 
plot(tt/365, obs_pr, type = "b", xlab = "Time (in years)", ylab = "PR", ylim = c(0,1))
lines(tt/365, pr, lwd=2) 

## ----shell--------------------------------------------------------------------
mod  <- xds_setup_cohort(season_par = makepar_F_sin())

## ----pr2eir_history-----------------------------------------------------------
mod1 <- pr2eir_history(obs_pr, tt, mod, twice=FALSE)

## -----------------------------------------------------------------------------
Fx <- mod1$EIRpar$F_trend
integrate(Fx,min(tt), max(tt))$val/(max(tt)-min(tt)) 

## -----------------------------------------------------------------------------
mod2 <- pr2eir_history(obs_pr, tt, mod, twice=TRUE)

## ----vis----------------------------------------------------------------------
ttn = seq(-365, max(tt), length.out=600)
xds_plot_PR(mod1)
xds_plot_PR(mod2, clrs="blue", add=TRUE)
lines(tt, obs_pr, xlab = "Time (in years)", ylab = "PR")
lines(tt, pr, col = "red")  

