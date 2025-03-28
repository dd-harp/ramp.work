
#' @title Estimate the effect sizes of vector control
#' @description
#' This function fits effect sizes for vector-based interventions. Each
#' intervention in a list includes a temporal profile describing how the
#' *effect* of the intervention wanes over time and the inception time.
#' Each model of intervention -- IRS or ITNs -- has its own sub-model that
#' describes how bionomic parameters are modified by vector control, given
#' contact and the waning effects.
#'
#' @param model a **`ramp.xds`** model object
#' @param base_ix counterfactual baseline knots
#' @param data the PR observed
#' @param times the times of the observations
#' @param irs_rounds a list to configure irs_multiround
#' @param itn_rounds a list to configure itn_multiround
#' @export
estimate_effect_sizes = function(model, base_ix, data, times, irs_rounds = list(), itn_rounds=list()){

  if(length(irs_rounds)>0){n_irs_zap = length(irs_rounds$zap)}else{n_irs_zap=0}
  if(length(itn_rounds)>0){n_itn_zap = length(itn_rounds$zap)}else{n_itn_zap=0}
  n_zap = n_irs_zap + n_itn_zap
  stopifnot(n_zap > 0)

  yy <- model$Lpar[[1]]$trend_par$yy
  yy[base_ix] <- gam_sample(yy[-base_ix], length(base_ix))

  x_init <- rep(.5, n_zap)
  X <- stats::optim(x_init, sse_effect_sizes,
                    data=data, times=times, model=model,
                    irs_rounds=irs_rounds, itn_rounds=itn_rounds)$par
  effect_sizes_set_zap(X, model, irs_rounds, itn_rounds)
  return(model)
}

#' @title Given data, compute GoF for a spline function
#' @description For a time series c(`times`,`data`),
#' compute the sum of squared errors for a seasonal
#' pattern defined by `Fpar`
#' @param data the PR observed
#' @param times the times of the observations
#' @param model the model
#' @param irs_rounds a list to configure irs_multiround
#' @param itn_rounds a list to configure itn_multiround
#' @return a list with the mean peak and the values
#' @export
sse_effect_sizes <- function(X, data, times, model, irs_rounds, itn_rounds){
  model <- effect_sizes_set_zap(X, model, irs_rounds, itn_rounds)
  model <- xds_solve(model, times=times)
  pr <- get_XH(model)$true_pr[-1]
  return(sum((data - pr)^2))
}


#' @title Set the zap parameters
#' @description Set the zap parameters
#' to fit effect sizes
#'
#' @param X a set of parameters to be fit
#' @param model a **`ramp.xds`** model object
#' @param irs_rounds a list to configure irs_multiround
#' @param itn_rounds a list to configure itn_multiround
#' @export
effect_sizes_set_zap = function(X, model, irs_rounds, itn_rounds){
  if(length(irs_rounds)==0){n_irs_zap=0}else{
    n_irs_zap=length(irs_rounds$zap)
    irs_rounds$zap = X[1:n_irs_zap]
    multiround = setup_irs_multiround(opts=irs_rounds)
    model$irs$coverage$trend_par <- irs_rounds
    model$irs$coverage$F_trend <- make_function(multiround)
  }
  if(length(itn_rounds)==0){n_itn_zap=0} else{
    n_itn_zap = length(itn_rounds$zap)
    irs_rounds$zap = X[n_irs_zap + 1:n_itn_zap]
    multiround = setup_itn_multiround(opts=itn_rounds)
    model$itn$coverage$trend_par <- itn_rounds
    model$bednets$coverage$F_trend <- make_function(multiround)
  }
  return(model)
}
