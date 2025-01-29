## -----------------------------------------------------------------------------
library(ramp.xds)
library(ramp.work)
devtools::load_all()

## -----------------------------------------------------------------------------
tt <- round(seq(0, 2500, by = 365/12)) 
Fpar <- makepar_F_sin(pw=3, floor=0.3, phase=93)
Tpar <- makepar_F_splinef(c(0:7)*365, c(1,2,1,1/2,1,3,1,1/3))
Fs0 <- make_function(Fpar) 
Ts0 <- make_function(Tpar) 
plot(tt/365, Fs0(tt)*Ts0(tt), type = "l", xlab = "Time (Years)", ylab = "EIR")

## -----------------------------------------------------------------------------
mod  <- xds_setup_cohort(1/365, F_season = Fs0, F_trend= Ts0)
mod  <- xds_solve_cohort(mod, times=tt)  
pr <- mod$outputs$orbits$XH[[1]]$true_pr 

## -----------------------------------------------------------------------------
obs_pr <- pmax(0, rnorm(length(tt), pr, .08))
xds_plot_PR(mod)
lines(tt, obs_pr)

## -----------------------------------------------------------------------------
mod1 <- xds_setup_cohort(eir = 1/365, F_season = Fs0) 
mod1 <- xds_solve_cohort(mod1, times=tt)
xds_plot_PR(mod1)
lines(tt, obs_pr)

## -----------------------------------------------------------------------------
fit_splinef(8, obs_pr, tt, mod1) -> Ts1

## -----------------------------------------------------------------------------
mod1$EIRpar$F_trend <- make_function(Ts1)

## -----------------------------------------------------------------------------
mod1 <- xds_solve_cohort(mod1, times=tt)
xds_plot_PR(mod1)
lines(tt, obs_pr)

## -----------------------------------------------------------------------------
mod2  <- xds_setup_cohort(F_season = make_function(makepar_F_sin()))
mod2 <- pr_ts2eir_history(obs_pr, tt, mod2)

## -----------------------------------------------------------------------------
mod2$fits$par_season

## -----------------------------------------------------------------------------
mod2$fits$par_trend

## -----------------------------------------------------------------------------
mod2 <- xds_solve_cohort(mod2, times=tt)
xds_plot_PR(mod2, clrs="darkred")
lines(tt, pr, lwd=2)
lines(tt, obs_pr, col = grey(0.2))
points(tt, obs_pr)

