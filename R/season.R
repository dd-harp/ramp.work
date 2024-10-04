#' @title Compute the phase of the peak
#' @description For a time series \eqn{X,} compute the
#' phase, the time of the year when there is a peak
#' @param t the times
#' @param X the data
#' @param window days around t
#' @export
mean_phase_peak = function(t, X, window=180){
  min_t = min(t); max_t = max(t)
  peak = c()
  for(i in 1:length(t)){
    t_ix = which(t>t[i]- window & t<t[i] + window)
    ix = which.max(X[t_ix])
    peak = c(peak, t[t_ix][ix] %% 365)
  }
  return(mean(peak))
}
