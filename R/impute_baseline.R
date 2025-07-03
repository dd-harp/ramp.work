
#' @title Impute the baseline
#' @description
#' This function modifies
#'
#' @param name the indices for the knots to be modified
#' @param model a **`ramp.xds`** model
#' @param ix the index of the node to replace
#'
#' @returns an object to dispatch
#' @export
impute_baseline = function(name, model, ix){
  class(name) <- name
  UseMethod("impute_baseline", name)
}

#' @title Use Mean for Modify Baseline
#' @description
#' This function modifies
#'
#' @inheritParams impute_baseline
#'
#' @returns an object to dispatch [modify_baseline.ttyy]
#' @export
impute_baseline.mean = function(name, model, ix){
  yy <- head(model$Lpar[[1]]$trend_par$yy, ix-1)
  tt <- head(model$Lpar[[1]]$trend_par$tt, ix-1)
  tpos <- which(tt>0)
  ymean <- mean(yy[tpos])
  model$Lpar[[1]]$trend_par$yy[ix] = ymean
  model$Lpar[[1]]$F_trend = make_function(model$Lpar[[1]]$trend_par)
  return(model)
}

#' @title Use Max for Modify Baseline
#' @description
#' This function modifies
#'
#' @inheritParams impute_baseline
#'
#' @returns an object to dispatch
#' @export
impute_baseline.max = function(name, model, ix){
  yy <- head(model$Lpar[[1]]$trend_par$yy, ix-1)
  tt <- head(model$Lpar[[1]]$trend_par$tt, ix-1)
  tpos <- which(tt>0)
  ymax <- max(yy[tpos])
  model$Lpar[[1]]$trend_par$yy[ix] = ymax
  model$Lpar[[1]]$F_trend = make_function(model$Lpar[[1]]$trend_par)
  return(model)
}

#' @title Use Min for Modify Baseline
#' @description
#' This function modifies
#'
#' @inheritParams impute_baseline
#'
#' @returns an object to dispatch [modify_baseline.ttyy]
#' @export
impute_baseline.min = function(ix, model){
  yy <- head(model$Lpar[[1]]$trend_par$yy, ix-1)
  tt <- head(model$Lpar[[1]]$trend_par$tt, ix-1)
  tpos <- which(tt>0)
  ymin <- max(yy[tpos])
  model$Lpar[[1]]$trend_par$yy[ix] = ymin
  model$Lpar[[1]]$F_trend = make_function(model$Lpar[[1]]$trend_par)
  return(model)
}

#' @title Use Median for Modify Baseline
#' @description
#' This function modifies
#'
#' @inheritParams impute_baseline
#'
#' @returns an object to dispatch [modify_baseline.ttyy]
#' @export
impute_baseline.median = function(ix, model){
  yy <- head(model$Lpar[[1]]$trend_par$yy, ix-1)
  tt <- head(model$Lpar[[1]]$trend_par$tt, ix-1)
  tpos <- which(tt>0)
  ymedian <- max(yy[tpos])
  model$Lpar[[1]]$trend_par$yy[ix] = ymedian
  model$Lpar[[1]]$F_trend = make_function(model$Lpar[[1]]$trend_par)
  return(model)
}

#' @title Baseline with gamma predictions
#' @description
#' This function modifies
#'
#' @inheritParams impute_baseline
#'
#' @returns an object to dispatch [modify_baseline.ttyy]
#' @export
impute_baseline.gam = function(ix, model){
  yy <- head(model$Lpar[[1]]$trend_par$yy, ix-1)
  tt <- head(model$Lpar[[1]]$trend_par$tt, ix-1)
  tpos <- which(tt>0)
  model$Lpar[[1]]$trend_par$yy[ix] <- gam_sample(yy[tpos], 1)
  model$Lpar[[1]]$F_trend = make_function(model$Lpar[[1]]$trend_par)
  return(model)
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
impute_baseline.subsamp = function(ix, model){
  yy <- head(model$Lpar[[1]]$trend_par$yy, ix-1)
  tt <- head(model$Lpar[[1]]$trend_par$tt, ix-1)
  tpos <- which(tt>0)
  model$Lpar[[1]]$trend_par$yy[ix] = sample(yy[tpos], 1)
  model$Lpar[[1]]$F_trend = make_function(model$Lpar[[1]]$trend_par)
  return(model)
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
impute_baseline.linear = function(ix, model){
  yy <- head(model$Lpar[[1]]$trend_par$yy, ix-1)
  tt <- head(model$Lpar[[1]]$trend_par$tt, ix-1)
  yyv <- yy[which(tt>0)]
  llm <- lm(yyv ~ c(1:length(yyv)))
  model$Lpar[[1]]$trend_par$yy[ix] = tail(yyv, 1) + llm$coef[2]
  model$Lpar[[1]]$F_trend = make_function(model$Lpar[[1]]$trend_par)
  return(model)
}
