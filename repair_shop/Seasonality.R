## -----------------------------------------------------------------------------
library(ramp.xds)
library(ramp.work)

## -----------------------------------------------------------------------------
#devtools::load_all()

## -----------------------------------------------------------------------------
tt <- round(seq(0, 2500, by = 365/12)) 
Fpar <- makepar_F_sin(pw=3, bottom=0.3, phase=172)
Fs0 <- make_function(Fpar) 
Fpar1 <- makepar_F_sin(pw=1, bottom=5, phase=172)
Fs1 <- make_function(Fpar1) 
plot(tt/365, Fs0(tt), type = "l", xlab = "Time (Years)", ylab = "EIR")
lines(tt/365, Fs1(tt))

## -----------------------------------------------------------------------------
mod  <- xds_setup_cohort(1/365, F_season = Fs0)
mod  <- xds_solve_cohort(mod, times=tt)  
pr <- mod$outputs$orbits$XH[[1]]$true_pr 

## -----------------------------------------------------------------------------
modn <- xds_setup_cohort(1/365, F_season = Fs1)
modn  <- xds_solve_cohort(modn, times=tt)  
prn <- modn$outputs$orbits$XH[[1]]$true_pr 


## -----------------------------------------------------------------------------
obs_pr <- pmax(0, rnorm(length(tt), pr, .08))
xds_plot_PR(mod)
lines(tt, obs_pr)

## -----------------------------------------------------------------------------
obs_prn <- pmax(0, rnorm(length(tt), prn, .08))
xds_plot_PR(modn, clrs="darkblue")
lines(tt, obs_prn, col = "darkblue")

## -----------------------------------------------------------------------------
mod1 <- xds_setup_cohort(eir = 1/365) 
mod1 <- xds_solve_cohort(mod1, times=tt)
mod2 <- mod1

## -----------------------------------------------------------------------------
fit_EIR_phase_sin_season(obs_pr, tt, mod1) -> mod1 
mod1$EIRpar$season_par$phase 
fit_EIR_phase_sin_season(obs_prn, tt, mod2) -> mod2 
mod2$EIRpar$season_par$phase 

## -----------------------------------------------------------------------------
mod1 <- fit_EIR_amplitude_sin_season(obs_pr, tt, mod1)
mod1$EIRpar$season_par
mod2 <- fit_EIR_amplitude_sin_season(obs_prn, tt, mod2)
mod2$EIRpar$season_par

## -----------------------------------------------------------------------------
fit_EIR_phase_sin_season(obs_pr, tt, mod1) -> mod1 
fit_EIR_amplitude_sin_season(obs_pr, tt, mod1) -> mod1

## -----------------------------------------------------------------------------
mod1 <- xds_solve_cohort(mod1, times=tt)

xds_plot_PR(mod)
xds_plot_PR(mod1, clrs="darkred", add=TRUE)
lines(tt, obs_pr)

## -----------------------------------------------------------------------------
mod2 <- xds_solve_cohort(mod2, times=tt)

xds_plot_PR(modn)
xds_plot_PR(mod2, clrs="darkred", add=TRUE)
lines(tt, obs_prn)

