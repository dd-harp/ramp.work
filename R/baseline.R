
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
#' @param data the PR observed
#' @param times the times of the observations
#' @param irs_rounds a list to set up multiround irs coverage
#' @param bednets_rounds a list to set up multiround bednets coverage
#' @param impute_by a string to dispatch the imputation method
#' @export
reconstruct_baseline = function(model, data, times, irs_rounds, bednets_rounds, impute_by ="mean"){

  model <- setup_irs(model,
                     coverage_name = "multiround",
                     coverage_opts = irs_rounds,
                     effect_sizes_name = "simple")

  model <- setup_bednets(model,
                     coverage_name = "multiround",
                     coverage_opts = bednets_rounds,
                     effect_sizes_name = "lemenach")

  model <- make_vc_event_list(model)

  for(i in 1:model$vc_events$nEvents){
    print(c(i=i))
    model = impute_baseline(impute_by, model, model$vc_events$events[[i]]$ix)
    print("effect size")
    model = estimate_effect_size(model, data, times, model$vc_events$events[[i]])
  }

  return(model)
}

#' @title Make an events list
#' @description
#' Return an ordered list of events by time and type
#'
#' @param model a **`ramp.xds`** model object
#' @return a modified  **`ramp.xds`** model object
#' @export
make_vc_event_list = function(model){

  knots <- work_mod$Lpar[[1]]$trend_par$tt

  event_list = list()

  t_init = model$irs$coverage_mod$t_init
  times = t_init
  n = length(t_init)
  type = rep("irs", n)
  knot_n = rep(0, n)

  for(i in 1:n){
    ev <- list()
    ev$name <- "irs"
    class(ev) <- "irs"
    ev$t <- t_init[i]
    ev$conflict <- FALSE
    ev$i <- i
    ev$other <- c()
    knot_n[i] <- min(which(ev$t<knots))
    ev$ix <- knot_n[i]
    event_list[[i]] = ev
  }

  t_init = model$bednets$coverage_mod$t_init
  m = length(t_init)
  knot_m = rep(0, m)
  type = c(type, rep("bednets", m))
  for(i in 1:m){
    ev <- list()
    ev$name <- "bednets"
    class(ev) <- "bednets"
    ev$t <- t_init[i]
    ev$i <- i
    ev$conflict <- FALSE
    ev$other <- c()
    knot_m[i] <- min(which(ev$t<knots))
    ev$ix <- knot_m[i]
    event_list[[i+n]] = ev
  }


  times = c(times, t_init)

  ot <- order(times)
  events = event_list
  knots = c(knot_n,knot_m)[ot]
  for(i in 1:n){
    events[[i]] = event_list[[ot[i]]]
  }
  conflicts=rep(FALSE, n+m)

  times <- times[ot]
  type <- type [ot]
  ix = which(diff(times) < 365)
  if(length(ix)>0)
    for(i in ix){
      conflicts[i] = TRUE
      conflicts[i+1] = TRUE
      events[[i]]$conflict = TRUE
      events[[i]]$other = c(events[[i]]$other,i+1)
      events[[i+1]]$conflict = TRUE
      events[[i+1]]$other = c(events[[i+1]]$other,i)
    }

  model$vc_events <- list(summary=data.frame(time = times, ix=knots, type=type , conflict =conflicts), events=events, nEvents = n+m)
  return(model)
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
#' @param model a **`ramp.xds`** model object
#' @param data the PR observed
#' @param times the times of the observations
#' @param event relevant details of an event to assess
#' @export
estimate_effect_size = function(model, data, times, event){
  UseMethod("estimate_effect_size", event)
}

#' @title Given data, compute GoF for a spline function
#' @description For a time series c(`times`,`data`),
#' compute the sum of squared errors for a seasonal
#' pattern defined by `Fpar
#' @param X the value to be tested
#' @param data the PR observed
#' @param times the times of the observations
#' @param model the model
#' @param event relevant details of an event to assess
#' @return a list with the mean peak and the values
#' @export
sse_effect_size <- function(X, data, times, model, event){
  UseMethod("sse_effect_size", event)
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
#'
#' @export
estimate_effect_size.irs = function(model, data, times, event){

  X <- stats::optimize(sse_effect_size, c(0,100),
                       data=data, times=times, model=model, event=event)$minimum

  model$irs$coverage_mod$zap[event$i] <- X
  model$irs$coverage_mod$rounds[[event$i]]$zap <- X
  rounds_par <- with(model$irs$coverage_mod, makepar_F_multiround(nRounds, rounds))
  model$irs$coverage_mod$rounds_par = rounds_par
  model$irs$coverage_mod$F_cover = make_function(rounds_par)

  return(model)
}

#' @title Given data, compute GoF for a spline function
#' @description For a time series c(`times`,`data`),
#' compute the sum of squared errors for a seasonal
#' pattern defined by `Fpar`
#'
#' @inheritParams sse_effect_size
#'
#' @return a list with the mean peak and the values
#' @export
sse_effect_size.irs <- function(X, data, times, model, event){

  model$irs$coverage_mod$rounds[[event$i]]$zap <- X
  rounds_par <- with(model$irs$coverage_mod, makepar_F_multiround(nRounds, rounds))
  model$irs$coverage_mod$F_cover = make_function(rounds_par)

  model <- xds_solve(model, times=c(0,times))
  pr <- get_XH(model)$true_pr[-1]
  return(-sum((data - pr)^2))
#  return(sum((data - pr)^2))
}

#' @title Estimate the effect size for a single round of mass bed net distribution
#' @description
#' This function fits the effect size for a bed net distribution.
#'
#' @inheritParams estimate_effect_size
#'
#' @export
estimate_effect_size.bednets = function(model, data, times, event){


  X <- stats::optimize(sse_effect_size, c(0,100),
                       data=data, times=times, model=model, event=event)$minimum

  model$bednets$coverage_mod$zap[event$i] <- X
  model$bednets$coverage_mod$rounds[[event$i]]$zap <- X
  rounds_par <- with(model$bednets$coverage_mod, makepar_F_multiround(nRounds, rounds))
  model$bednets$coverage_mod$rounds_par = rounds_par
  model$bednets$coverage_mod$F_cover = make_function(rounds_par)

  return(model)
}

#' @title Given data, compute GoF for a spline function
#' @description For a time series c(`times`,`data`),
#' compute the sum of squared errors for a seasonal
#' pattern defined by `Fpar`
#'
#' @inheritParams sse_effect_size
#'
#' @return a list with the mean peak and the values
#' @export
sse_effect_size.bednets <- function(X, data, times, model, event){


  model$bednets$coverage_mod$rounds[[event$i]]$zap <- X
  rounds_par <- with(model$bednets$coverage_mod, makepar_F_multiround(nRounds, rounds))
  model$bednets$coverage_mod$F_cover = make_function(rounds_par)

  model <- xds_solve(model, times=c(0,times))
  pr <- get_XH(model)$true_pr[-1]

  return(-sum((data - pr)^2))
#  return(sum((data - pr)^2))
}
