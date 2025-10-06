
#' @title Compute Measures of Impact
#'
#' @param history a model of the history: a **`ramp.xds`** xds_obj object
#' @param counterfactual a model of the counterfactual: a **`ramp.xds`** xds_obj
#' @param times a set of times to simulate
#' @param event_date the date of an event
#' @param eval_period the date of an event
#'
#' @returns a list with measures of impact
#'
#' @export
compute_impact = function(history, counterfactual, times=c(), event_date = c(), eval_period=365){
  if(length(times) == 0){
    first_day <- min(history$data$jdates)
    last_day <- max(history$data$jdates)
    t0 = history$fitting$t_neg_inf
    times <- c(t0, first_day:last_day)
  }
  if(length(event_date) == 0){
    eval=times
  } else {
    eval = event_date + 1:eval_period
  }

  history <- xds_solve(history, times=times)
  counterfactual <- xds_solve(counterfactual, times=times)
  pr_hist = get_PR(history)[-1]
  pr_base = get_PR(counterfactual)[-1]
  pr_averted_ts = pr_base[eval] - pr_hist[eval]
  d_pr_max = max(pr_averted_ts)
  d_pr_mean = mean(pr_averted_ts)
  list(pr_hist=pr_hist, pr_base=pr_base,
       pr_averted_ts=pr_averted_ts,
       d_pr_max=d_pr_max,
       d_pr_mean=d_pr_mean,
       t_eval=eval,
       event_date=event_date)
}
