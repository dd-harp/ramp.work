## -----------------------------------------------------------------------------
suppressMessages(library(ramp.xds))
suppressMessages(library(ramp.work))

## ----echo=F-------------------------------------------------------------------
devtools::load_all()

## -----------------------------------------------------------------------------
set.seed(136)
tt0 <-  365*(0:6)
yy0 <-  c(.7, 2, 1.8, .2, .3, 1.4, 1.1)
mod1 <- xds_setup_cohort(
  eir = 2/365,
  season_par = makepar_F_sin(bottom=.1),
  trend_par = makepar_F_spline(tt=tt0, yy=yy0)
)
tt <- seq(0, 365*6, by = 30)
mod1 <- xds_solve_cohort(mod1, times=tt)
mod1 <- last_to_inits(mod1)
mod1 <- xds_solve_cohort(mod1, times=tt)
true_pr <- get_XH(mod1)$true_pr
obs_pr = true_pr*rlnorm(length(tt), 0, .3)

## ----fig.width=7, fig.height=4, echo=F----------------------------------------
xds_plot_PR(mod1, clrs="darkblue")
lines(tt, obs_pr, col="darkblue", type = "b", pch = 15)
segments(900, 0, 900, 1, col = "darkred")
segments(1265, 0, 1265, 1, col = "darkred")

## -----------------------------------------------------------------------------
ix = c(4,5)
yy1 <- yy2 <- yy0 
yy1[ix] <- gam_sample(yy0[-ix], 2)
yy2[ix] <- gam_sample(yy0[-ix], 2)
rbind(yy1, yy2)

## -----------------------------------------------------------------------------
mod1b <- mod1
mod2b <- mod1
mod1b$EIRpar$trend_par <- makepar_F_spline(tt=tt0, yy=yy1)
mod2b$EIRpar$trend_par <- makepar_F_spline(tt=tt0, yy=yy2)
mod1b <- xds_solve_cohort(mod1b, times=tt)
mod2b <- xds_solve_cohort(mod2b, times=tt)
base_pr <- get_XH(mod1b)$true_pr
xds_plot_PR(mod1b)
xds_plot_PR(mod2b, clrs = "darkgreen", add=TRUE)
xds_plot_PR(mod1, clrs="darkblue", add=TRUE) 
lines(tt, obs_pr, col = "darkblue", type = "b", pch=15)

## -----------------------------------------------------------------------------
Lo <- list(
  Lambda = 50,
  season_par = makepar_F_sin(),
  trend_par = makepar_F_spline(tt0, tt0*0+1)
)

## -----------------------------------------------------------------------------
mod <- xds_setup(Lopts = Lo)
mod <- setup_travel_eir(mod, travelEIR = 1/3650)

## -----------------------------------------------------------------------------
mod <- xds_solve(mod, 3650, 3650)
mod <- last_to_inits(mod)
mod <- xds_solve(mod)
xds_plot_PR(mod)

## ----eval=F-------------------------------------------------------------------
#  mod <- pr2Lambda_history(obs_pr, tt, mod, twice=TRUE)

## -----------------------------------------------------------------------------
xds_plot_PR(mod)

