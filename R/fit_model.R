#' Fit a model to data
#'
#' @param xds_obj a **`ramp.xds`** model object
#' @param update dispatch string(s)
#' @param ix indices, for model fitting
#'
#' @return a list with the mean peak and the values
#' @export
fit_model <- function(xds_obj, update, ix){

  stopifnot(length(ix)==length(update))
  if(length(update)>1) class(update) = "multifit"

  Xinits = get_init_X(xds_obj, update, ix)
  inits = unlist(Xinits)
  xds_obj$fitting$Xinits = Xinits

  if(length(Xinits)==1){
    lims = get_limits_X(xds_obj, update)
    fitit <- stats::optimize(compute_gof_X, lims,
                             xds_obj=xds_obj, ix=1, update = update)
    X <- fitit$minimum
  } else {
    fitit <- stats::optim(inits, compute_gof_X,
                          update=update, ix=ix, xds_obj=xds_obj)
    X <- fitit$par
  }
  xds_obj <- update_function_X(X, xds_obj, update, ix)
  xds_obj <- burnin(xds_obj)
  return(xds_obj)
}

#' Compute the GoF for `X`
#'
#' @inheritParams get_init_X
#'
#' @returns sum of squared differences
#' @export
get_init_X.multifit = function(xds_obj, update, ix){
  Xinits = list()
  for(i in 1:length(update))
    Xinits[[i]] = get_init_X(xds_obj, update[i], ix[[i]])
  return(Xinits)
}

#' Compute the GoF for `X`
#'
#' @inheritParams compute_gof_X
#'
#' @returns sum of squared differences
#' @export
update_function_X.multifit = function(X, xds_obj, update, ix){
  stopifnot(length(update) == length(xds_obj$fitting$Xinits))
  stopifnot(length(update) == length(ix))
  ixx = 0
  for(i in 1:length(update)){
    ixx = 1:length(xds_obj$fitting$Xinits[[i]]) + max(ixx)
    xds_obj <- update_function_X(X[ixx], xds_obj, update[i], ix[[i]])
  }
  return(xds_obj)
}


#' @title Plot the model and the data
#'
#' @param xds_obj a **`ramp.xds`** model object
#' @param add add to an existing show_fit plot
#' @param clr the color of the line to be plotted
#'
#' @returns the PR, invisibly
#' @export
show_fit = function(xds_obj, clr = "black", add=FALSE){
  if(add==FALSE)
    with(xds_obj$data, plot(jdates, pfpr, type = "p", xlab = "Time", ylab = "PfPR", pch=15, ylim = range(0, pfpr)))
  xds_obj <- ramp.xds::xds_solve(xds_obj, times = c(0, xds_obj$data$jdates))
  ramp.xds::xds_plot_PR(xds_obj, clr=clr, add=TRUE)
  return(invisible(get_PR(xds_obj)))
}





