
#' @title Modify the Baseline
#' @description
#' This function modifies the parameters
#' in a model to set up a counter
#'
#' @param model a **`ramp.xds`** model object
#' @param counterfactual a list
#'
#' @returns a **`ramp.xds`** model
#' @export
modify_baseline = function(model, counterfactual){
  UseMethod("modify_baseline", model$frame)
}


#' @title Create a modify Baseline
#' @description
#' This function modifies
#'
#' @inheritParams modify_baseline
#'
#' @returns a **`ramp.xds`** model
#' @export
modify_baseline.full = function(model, counterfactual){
  with(counterfactual,{
    model$Lpar[[1]]$trend_par$yy[knot_ix] <- cc_yy
    model$Lpar[[1]]$F_trend <- make_function(model$Lpar[[1]]$trend_par)
    model <- xds_solve(model, times = model$outputs$times)
    return(model)
})}

#' @title Create a modify Baseline
#' @description
#' This function modifies
#'
#' @inheritParams modify_baseline
#'
#' @returns a **`ramp.xds`** model
#' @export
modify_baseline.cohort = function(model, counterfactual){
  with(counterfactual,{
    model$EIRpar$trend_par$yy[knot_ix] <- cc_yy
    model$EIRpar$F_trend <- make_function(model$EIRpar$trend_par)
    model <- xds_solve_cohort(model, times = model$outputs$times)
    return(model)
})}

#' @title Create a modify Baseline
#' @description
#' This function modifies
#'
#' @inheritParams modify_baseline
#'
#' @returns a function object
#' @export
modify_baseline.func= function(model, counterfactual){
  with(counterfactual,{
    model$Tpar$yy[knot_ix] <- cc_yy
    return(model)
  })}

#' @title Modify the Baseline
#' @description
#' This function modifies the parameters
#' in a model to set up a counter
#'
#' @param model a **`ramp.xds`** model object
#'
#' @returns a [vector]
#' @export
get_spline_yy = function(model){
  UseMethod("get_spline_yy", model$frame)
}

#' @title Modify the Baseline
#' @description
#' This function modifies the parameters
#' in a model to set up a counter
#'
#' @param model a **`ramp.xds`** model object
#' @param counterfactual
#'
#' @returns a [vector]
#' @export
get_spline_yy.full = function(model){
  model$Lpar[[1]]$trend_par$yy
}

#' @title Modify the Baseline
#' @description
#' This function modifies the parameters
#' in a model to set up a counter
#'
#' @param model a **`ramp.xds`** model object
#' @param counterfactual
#'
#' @returns a [vector]
#' @export
get_spline_yy.cohort = function(model){
  model$EIRpar$trend_par$yy
}

#' @title Modify the Baseline
#' @description
#' This function modifies the parameters
#' in a model to set up a counter
#'
#' @inheritParams get_spline_yy
#'
#' @returns a [vector]
#' @export
get_spline_yy.func= function(model){
  model$Tpar$yy
}


#' @title Pass Values for Modify Baseline
#' @description
#' This function modifies
#'
#' @param ix the indices for the knots to be modified
#' @param yy the new control points
#'
#' @returns an object to dispatch [modify_baseline.ttyy]
#' @export
make_cf_base_ttyy = function(ix, yy){
  baseline <- list()
  class(baseline) <- "ttyy"
  baseline$knot_ix = ix
  baseline$cc_yy = yy
  return(baseline)
}

#' @title Use Max for Modify Baseline
#' @description
#' This function modifies
#'
#' @param ix the indices for the knots to be modified
#' @param model a **`ramp.xds`** model
#'
#' @returns an object to dispatch
#' @export
make_cf_base_max = function(ix, model){
  yy <- get_spline_yy(model)
  ymax <- max(yy[-ix])
  make_cf_base_ttyy(ix, rep(ymax, length(ix)))
}

#' @title Use Max for No Unmodified Baseline
#' @description
#' This function modifies
#'
#' @param ix the indices for the knots to be modified
#' @param model a **`ramp.xds`** model
#'
#' @returns an object to dispatch
#' @export
make_cf_no_base = function( model){
  yy <- get_spline_yy(model)
  ymax <- max(yy)
  ix = 1:length(yy)
  make_cf_base_ttyy(ix, rep(ymax, length(ix)))
}

#' @title Use Min for Modify Baseline
#' @description
#' This function modifies
#'
#' @param ix the indices for the knots to be modified
#' @param model a **`ramp.xds`** model
#'
#' @returns an object to dispatch [modify_baseline.ttyy]
#' @export
make_cf_base_min = function(ix, model){
  yy <- get_spline_yy(model)
  ymin <- min(yy[-ix])
  make_cf_base_ttyy(ix, rep(ymin, length(ix)))
}


#' @title Use Mean for Modify Baseline
#' @description
#' This function modifies
#'
#' @param ix the indices for the knots to be modified
#' @param model a **`ramp.xds`** model
#'
#' @returns an object to dispatch [modify_baseline.ttyy]
#' @export
make_cf_base_mean = function(ix, model){
  yy <- get_spline_yy(model)
  ymean <- mean(yy[-ix])
  make_cf_base_ttyy(ix, rep(ymean, length(ix)))
}

#' @title Use Median for Modify Baseline
#' @description
#' This function modifies
#'
#' @param ix the indices for the knots to be modified
#' @param model a **`ramp.xds`** model
#'
#' @returns an object to dispatch [modify_baseline.ttyy]
#' @export
make_cf_base_median = function(ix, model){
  yy <- get_spline_yy(model)
  ymed <- median(yy[-ix])
  make_cf_base_ttyy(ix, rep(ymed, length(ix)))
}

#' @title Baseline with gamma predictions
#' @description
#' This function modifies
#'
#' @param ix the indices for the knots to be modified
#' @param model a **`ramp.xds`** model
#'
#' @returns an object to dispatch [modify_baseline.ttyy]
#' @export
make_cf_base_gam = function(ix, model){
  yy <- get_spline_yy(model)
  yy_new = gam_sample(yy[-ix], length(ix))
  make_cf_base_ttyy(ix, yy_new)
}

#' @title Create a modify Baseline
#' @description
#' This function modifies
#'
#' @param ix the indices for the knots to be modified
#' @param model a **`ramp.xds`** model
#'
#' @returns an object to dispatch [modify_baseline.ttyy]
#' @export
make_cf_base_subsamp = function(ix, model){
  yy <- get_spline_yy(model)
  yy_new = sample(yy[-ix], length(ix), replace=TRUE)
  make_cf_base_ttyy(ix, yy_new)
}

