

#' @title Reconstruct History as a Baseline Modified by Control
#' @description
#' This function fits effect sizes for vector-based interventions. Each
#' intervention in a list includes a temporal profile describing how the
#' *effect* of the intervention wanes over time and the inception time.
#' Each model of intervention -- IRS or ITNs -- has its own sub-model that
#' describes how bionomic parameters are modified by vector control, given
#' contact and the waning effects.
#'
#' @param model a **`ramp.xds`** model object
#' @param pfpr_ts the *Pf*PR time series
#' @param jdates the times of the observations
#'
#' @param irs_history history of irs: jdate of spray round; insecticide name
#' @param bednet_history history of bednets: jdate of mass distribution; type bednets
#'
#' @param impute_ty a text string to dispatch `impute_baseline_ty`
#' @param trust_ty a text string to dispatch `get_trust_ty`
#' @export
reconstruct_baseline = function(model, pfpr_ts, jdates,
                                irs_history, bednet_history,
                                impute_ty="mean", trust_ty="unmodified"){

  model <- setup_vc_eval(model, irs_history, bednet_history)

  with(model$vc_events,{
    for(i in 1:length(model$vc_events$t_init)){
      val = impute_value(t_ix[i], model, impute_ty, trust_ty)
      yy  =  model$data$yy[t_ix[i]]
      tm = t_init[i]
      print(c(i=i, tm=tm, yy=yy, t_ix = t_ix[i],
              val=round(1000*val)/1000,
              yy=round(1000*yy)/1000,
              t_init=round(1000*t_init[i]/1000),
              event=event[i]))
      if(t_ix[i] > 0)
        if(val > yy){
          model <- impute_baseline_ty(t_ix[i], model, impute_ty, trust_ty)
          model <- estimate_effect_size(i, model, pfpr_ts, jdates)
      }
    }

    model$baseline = list()
    model$baseline$data <- model$data
    model$baseline$fitting <- model$fitting
    model$baseline$hindcast <- model$hindcast
    model$baseline$forecast <- model$forecast

    return(model)
})}

#' @title Estimate the effect size for a single round of IRS
#' @description
#' This function fits effect sizes for vector-based interventions. Each
#' intervention in a list includes a temporal profile describing how the
#' *effect* of the intervention wanes over time and the inception time.
#' Each model of intervention -- IRS or ITNs -- has its own sub-model that
#' describes how bionomic parameters are modified by vector control, given
#' contact and the waning effects.
#'
#' @param event_ix the index of the event to evaluate
#' @param model a **`ramp.xds`** model object
#' @param pfpr_ts the PR observed
#' @param jdates the julian dates of the observations
#' @export
estimate_effect_size = function(event_ix, model, pfpr_ts, jdates){
  class(event_ix) <- model$vc_events[event_ix,]$event
  UseMethod("estimate_effect_size", event_ix)
}

#' @title Estimate the effect size for a single round of IRS
#' @description
#' This function fits effect sizes for vector-based interventions. Each
#' intervention in a list includes a temporal profile describing how the
#' *effect* of the intervention wanes over time and the inception time.
#' Each model of intervention -- IRS or ITNs -- has its own sub-model that
#' describes how bionomic parameters are modified by vector control, given
#' contact and the waning effects.
#'
#' @inheritParams estimate_effect_size
#' @importFrom ramp.control setup_F_cover_irs
#'
#' @export
estimate_effect_size.irs = function(event_ix, model, pfpr_ts, jdates){

   eval_irs_coverage <- function(X, pfpr_ts, times, model, event_ix){
      model$irs$coverage_mod$coverage[[event_ix]] <- X
      model$irs$coverage_mod <- setup_F_cover_irs(model$irs$coverage_mod)
      model <- xds_solve(model, times=times)
      compute_gof(model, pfpr_ts)
  }

  times = c(min(model$fitting$tt),jdates)

  fitit <- stats::optimize(eval_irs_coverage, c(0,1),
                       pfpr_ts=pfpr_ts, times=times, model=model, event_ix=event_ix)

  model$irs$coverage_mod$coverage[[event_ix]] <- fitit$minimum
  model$irs$coverage_mod <- setup_F_cover_irs(model$irs$coverage_mod)
  model <- xds_solve(model, times=c(0, jdates))

  return(model)
}

#' @title Estimate the effect size for a single round of mass bed net distribution
#' @description
#' This function fits the effect size for a bed net distribution.
#'
#' @inheritParams estimate_effect_size
#' @importFrom ramp.control setup_F_cover_bednet
#' @export
estimate_effect_size.bednet = function(event_ix, model, pfpr_ts, jdates){

  eval_bednet_coverage <- function(X, pfpr_ts, times, model, event_ix){
    model$bednet$coverage_mod$coverage[[event_ix]] <- X
    model$bednet$coverage_mod <- setup_F_cover_bednet(model$bednet$coverage_mod)
    model <- xds_solve(model, times=times)
    compute_gof(model, pfpr_ts)
  }

  times = c(min(model$fitting$tt),jdates)

  fitit <- stats::optimize(eval_bednet_coverage, c(0,1), event_ix=event_ix,
                       pfpr_ts=pfpr_ts, times=times, model=model)

  model$bednets$coverage_mod$coverage[[event_ix]] <- fitit$minimum
  model$bednets$coverage_mod <- setup_F_cover_bednet(model$bednets$coverage_mod)
  model <- xds_solve(model, times=c(0, jdates))

  return(model)
}
