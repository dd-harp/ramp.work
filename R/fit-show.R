

#' @title Plot the model and the data
#'
#' @param xds_obj a **`ramp.xds`** model object
#' @param clr the color of the line to be plotted
#' @param add add to an existing show_fit plot
#' @param rng if not null, the y axis limits
#' @param dom if not null, the x axis limits
#'
#' @returns the PR, invisibly
#' @export
show_fit = function(xds_obj, clr="black", add=FALSE, rng=NULL,dom=NULL){
  if(is.null(rng)) rng = range(0, xds_obj$data_obj$pfpr)*1.05
  if(is.null(dom)) dom = range(xds_obj$data_obj$jdates)*1.02
  gof <- round(compute_gof(xds_obj)*1e5)/1e5
  if(add==FALSE){
    with(xds_obj$data_obj, plot(jdates, pfpr, type = "p",
                            xaxt = "n",
                            xlab = "Time", ylab = "PfPR",
                            pch=15, main = gof, xaxt = "n",
                            xlim = dom, ylim = rng))
    with(xds_obj$data_obj, axis(1, ymesh, years))
  }
  xds_obj <- ramp.xds::xds_solve(xds_obj, times = c(-3650, xds_obj$data$jdates[1]))
  xds_obj <- last_to_inits(xds_obj)
  xds_obj <- ramp.xds::xds_solve(xds_obj, times = xds_obj$data$jdates)
  ramp.xds::xds_plot_PR(xds_obj, clr=clr, add=TRUE)
  return(invisible(get_PR(xds_obj)))
}

#' @title Plot the model and the data
#'
#' @param xds_obj a **`ramp.xds`** model object
#' @param clr the color of the line to be plotted
#' @param add add to an existing show_fit plot
#' @importFrom graphics segments
#'
#' @returns the PR, invisibly
#' @export
show_residuals = function(xds_obj, clr="black", add=FALSE){
  jdates = xds_obj$data$jdates
  xds_obj <- ramp.xds::xds_solve(xds_obj, times = c(0, jdates))
  residuals <- xds_obj$data$pfpr - get_PR(xds_obj, 1)[-1]
  if(add==FALSE)
    plot(jdates, residuals, type = "p", xlab = "Time", ylab = "PfPR", pch=15, ylim = range(-max(residuals), max(residuals)))
  segments(0, 0, max(jdates), 0)
  return(invisible(residuals))
}
