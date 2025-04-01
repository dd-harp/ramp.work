## -----------------------------------------------------------------------------
suppressMessages(library(ramp.xds))
suppressMessages(library(ramp.work))
suppressMessages(library(ramp.control))

## ----echo=F-------------------------------------------------------------------
devtools::load_all()

## -----------------------------------------------------------------------------
set.seed(136)
tt0 <-  365*(0:6)
yy0 <-  c(.7, 2, 1.8, .2, .3, 1.4, 1.1)
yy0 <- yy0/mean(yy0)
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
segments(1000, 0, 1000, 1, col = "darkred")
segments(1365, 0, 1365, 1, col = "darkred")

## -----------------------------------------------------------------------------
ix = c(4,5)
yy1 <- yy2 <- yy3 <- yy0
yy1[ix] <- gam_sample(yy0[-ix], 2)
yy2[ix] <- gam_sample(yy0[-ix], 2)
mv <- mean(yy0[-ix]) 
yy3[ix] <- rep(mv, 2)
rbind(yy1, yy2)
rbind(yy1, yy2, yy3)

## -----------------------------------------------------------------------------
mod1a <- mod1
mod1b <- mod1
mod1c <- mod1
mod1a$EIRpar$trend_par <- makepar_F_spline(tt=tt0, yy=yy1)
mod1b$EIRpar$trend_par <- makepar_F_spline(tt=tt0, yy=yy2)
mod1c$EIRpar$trend_par <- makepar_F_spline(tt=tt0, yy=yy3)
mod1a <- xds_solve_cohort(mod1a, times=tt)
mod1b <- xds_solve_cohort(mod1b, times=tt)
mod1c <- xds_solve_cohort(mod1c, times=tt)
base_pr <- get_XH(mod1c)$true_pr
xds_plot_PR(mod1a)
xds_plot_PR(mod1b, clrs = "darkgreen", add=TRUE)
xds_plot_PR(mod1c, clrs = "purple4", add=TRUE)
segments(1000, 0, 1000, 1, col = "darkred")
segments(1365, 0, 1365, 1, col = "darkred")
xds_plot_PR(mod1, clrs="darkblue", add=TRUE) 
lines(tt, obs_pr, col = "darkblue", type = "b", pch=15)

## -----------------------------------------------------------------------------
Lo <- list(
  Lambda = 50,
  season_par = makepar_F_sin(),
  trend_par = makepar_F_spline(tt0, tt0*0+1)
)

## -----------------------------------------------------------------------------
mod <- xds_setup(Lopts = Lo, MYZname = "SI")
mod <- setup_travel_eir(mod, travelEIR = 1/3650/5)
mod <- xds_solve(mod, 3650, 3650)
mod <- last_to_inits(mod)

## -----------------------------------------------------------------------------
mod <- pr2Lambda_history(obs_pr, tt, mod, twice=TRUE)

## -----------------------------------------------------------------------------
mod <- xds_solve(mod, 6*365, 10)

## -----------------------------------------------------------------------------
xds_plot_PR(mod, clrs = "darkred")
xds_plot_PR(mod1, clrs="darkblue", add=TRUE)
lines(tt, obs_pr, col = "darkblue", type = "b", pch=15)

## -----------------------------------------------------------------------------
irs1 <- list(
  t_init = c(1000, 1365), 
  irs_type = c("actellic", "bendiocarb"), 
  coverage = c(0.9, 0.9), 
  zap = c(0.5, .3) 
)
rounds <- setup_irs_multiround(opts=irs1)

## -----------------------------------------------------------------------------
mod_irs <- setup_irs(mod, effectsizes_name = "simple", 
                          coverage_name = "func", 
                          coverage_opts = list(mx=1, trend_par=rounds))

## -----------------------------------------------------------------------------
devtools::load_all()

## -----------------------------------------------------------------------------
mod_est <- estimate_effect_sizes(mod_irs, obs_pr[-1], tt, c(4,5), c(1.4, 1.4), irs_rounds=irs1)

## -----------------------------------------------------------------------------
mod_est <- xds_solve(mod_est, 2190, 10) 
xds_plot_PR(mod_est, clrs= "darkred")
#xds_plot_PR(mod, clrs = "darkred", add=TRUE)
xds_plot_PR(mod1, clrs="darkblue", add=TRUE)
lines(tt, obs_pr, col = "darkblue", type = "b", pch=15)

