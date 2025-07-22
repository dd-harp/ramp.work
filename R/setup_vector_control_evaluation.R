' Setup Vector Control for Evaluation
#'
#' @param model a **`ramp.xds`** model object
#' @param irs_history history of irs: jdate of spray round; insecticide name
#' @param bednet_history history of bednets: jdate of mass distribution; type bednets
#'
#' @returns a **`ramp.xds`** model object
#' @importFrom ramp.control setup_irs setup_bednets
#'
#' @export
setup_vc_eval = function(model, irs_history, bednet_history){

  irs_ix = which(irs_history$t_init > 0)
  model <- setup_irs(model,
                     coverage_name = "multiround",
                     coverage_opts =irs_history[irs_ix,],
                     effect_sizes_name = "simple")

  bednet_ix = which(bednet_history$t_init > 0)
  model <- setup_bednets(model,
                         coverage_name = "multiround",
                         coverage_opts = bednet_history[bednet_ix,],
                         effect_sizes_name = "lemenach")

  model <- make_vc_event_list(model, irs_history[irs_ix,], bednet_history[bednet_ix,])

  modified = model$data$years*0
  t_ix = model$vc_events$t_ix

  if(length(t_ix)>0) modified[t_ix] = 1
  model$data$modified = modified

  return(model)
}


#' @title Make an events list
#'
#' @description
#' Return an ordered list of events by time and type
#'
#' @param model a **`ramp.xds`** model object
#' @param irs_history history of irs: jdate of spray round; insecticide name
#' @param bednet_history history of bednets: jdate of mass distribution; type bednets
#'
#' @return a modified  **`ramp.xds`** model object
#' @export
make_vc_event_list = function(model, irs_history, bednet_history){
  events = rbind(irs_history, bednet_history)
  events$t_ix = ceiling(events$t_init/365)+1
  events$coverage = 0
  ot <- order(events$t_init)
  model$vc_events = events[ot,]
  return(model)
}

