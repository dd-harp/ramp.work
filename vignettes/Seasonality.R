## -----------------------------------------------------------------------------
library(ramp.xds)
#library(ramp.work)
devtools::load_all()

## -----------------------------------------------------------------------------
Fpar <- makepar_F_sin(pw=3, floor=0.1, phase=73)
Fs0 <- make_function(Fpar) 
tt <- round(seq(0, 2500, by = 365/12)) 
plot(tt, Fs0(tt), type = "l")

## -----------------------------------------------------------------------------
mod  <- xds_setup_cohort(1/365, F_season = Fs0)
mod  <- xds_solve_cohort(mod, times=tt)  

## -----------------------------------------------------------------------------
mod1 <- xds_setup_cohort(eir = 1/365) 
mod1 <- xds_solve_cohort(mod1, times=tt)

## -----------------------------------------------------------------------------
pr <- mod$outputs$orbits$XH[[1]]$true_pr 
obs_pr <- pmax(0, rnorm(length(tt), pr, .05))
xds_plot_PR(mod)
xds_plot_PR(mod1, clrs = "darkblue", add=TRUE)
lines(tt, obs_pr)

## -----------------------------------------------------------------------------
fit_phase_sin_season(obs_pr, tt, mod1) -> phase_fit
phase_fit

## -----------------------------------------------------------------------------
Fp0 <- makepar_F_sin(phase=phase_fit)
Fp_fit <- fit_amplitude_sin_season(Fp0, obs_pr, tt, mod1)
mod1$EIRpar$F_season <- make_function(Fp_fit)
mod1 <- xds_solve_cohort(mod1, times=tt)

## -----------------------------------------------------------------------------
xds_plot_PR(mod)
xds_plot_PR(mod1, clrs="darkred", add=TRUE)
lines(tt, obs_pr)

