## -----------------------------------------------------------------------------
library(ramp.xds)
#library(ramp.work)
devtools::load_all()

## -----------------------------------------------------------------------------
tt <- round(seq(0, 2500, by = 365/12)) 
Fpar <- makepar_F_sin(pw=3, floor=0.3, phase=172)
Fs0 <- make_function(Fpar) 
plot(tt/365, Fs0(tt), type = "l", xlab = "Time (Years)", ylab = "EIR")

## -----------------------------------------------------------------------------
mod  <- xds_setup_cohort(1/365, F_season = Fs0)
mod  <- xds_solve_cohort(mod, times=tt)  
pr <- mod$outputs$orbits$XH[[1]]$true_pr 

## -----------------------------------------------------------------------------
obs_pr <- pmax(0, rnorm(length(tt), pr, .08))
xds_plot_PR(mod)
lines(tt, obs_pr)

## -----------------------------------------------------------------------------
mod1 <- xds_setup_cohort(eir = 1/365) 
mod1 <- xds_solve_cohort(mod1, times=tt)

## -----------------------------------------------------------------------------
fit_phase_sin_season(obs_pr, tt, mod1) -> mod1 
mod1$EIRpar$season_par$phase 

## -----------------------------------------------------------------------------
mod1 <- fit_amplitude_sin_season(obs_pr, tt, mod1)
mod1$EIRpar$season_par

## -----------------------------------------------------------------------------
fit_phase_sin_season(obs_pr, tt, mod1) -> mod1 
fit_amplitude_sin_season(obs_pr, tt, mod1) -> mod1

## -----------------------------------------------------------------------------
mod1 <- xds_solve_cohort(mod1, times=tt)

xds_plot_PR(mod)
xds_plot_PR(mod1, clrs="darkred", add=TRUE)
lines(tt, obs_pr)

