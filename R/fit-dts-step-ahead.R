
#' @title Compute the attack rates for a set of strata for a step-ahead algorithm
#' @description Compute the sum of squared errors
#' @param pr next value of the PfPR
#' @param model a model
#' @param get_pr a function to compute PR, such as F_pr
#' @param n the number of times to iterate
#' @param tol the tolerance
#' @return the sum of squared errors
#' @export
dts_pr2ar_step = function(pr, model, get_pr, n=365, tol=1e-8){
  ar = stats::optimize(ar_compare, c(0,1), pr=pr, model=model, get_pr=get_pr, n=n, tol=tol)$minimum
  return(ar)
}

#' @title Compute the next y
#' @description Compute the sum of squared errors
#' @param ar the attack rate
#' @param model a model
#' @param n the number of times to iterate
#' @return a vector of state variables, y
#' @export
update_by_ar = function(ar, model, n=365){
  y = get_inits(model)
  for(i in 1:n){
    model$AR[[1]] = ar
    Xt = DT_Xt(t, y, model, 1)
    y <- put_Xvars(Xt, y, model, 1)
  }
  return(y)
}

#' @title Compute the attack rate to get
#' @description Compute the sum of squared errors
#' @param ar the attack rate
#' @param pr next value of the PfPR
#' @param get_pr a function to compute PR, such as F_pr
#' @param model a model
#' @param n the number of times to iterate
#' @return the sum of squared errors
#' @export
ar_compare = function(ar, pr, get_pr, model, n=1){
  y <- update_by_ar(ar, model, n)
  ppr = get_pr(list_Xvars(y, model, 1), model$Xpar[[1]])
  F_sse(pr, ppr)
}

#' @title Initialize a dts model such that
#' @description Compute the sum of squared errors
#' @param pr a PR time series
#' @param model a model
#' @param get_pr a functrion to compute PR, such as F_pr
#' @param n the number of times to iterate
#' @param tol the tolerance
#' @return the model, initialized
#' @export
dts_init_by_pr = function(pr, model, get_pr, n=365, tol=1e-8){
  ar = dts_pr2ar_step(pr, model, get_pr, n, tol)
  y = update_by_ar(ar, model)
  model = update_inits(y, model)
  err = 1
  while(err > tol){
    ar = dts_pr2ar_step(pr, model, get_pr, n, tol)
    y = update_by_ar(ar, model)
    model = update_inits(unname(y), model)
    err = ar_compare(ar, pr, get_pr, model)
  }
  return(model)
}

#' @title Compute the attack rate to get
#' @description Compute the sum of squared errors
#' @param pr a PR time series
#' @param model a model
#' @param get_pr a function to compute PR, such as F_pr
#' @param tol the tolerance
#' @return a vector of attack rates, as a time series
#' @export
dts_pr2ar_ts = function(pr, model, get_pr, tol=1e-8){

  model <- dts_init_by_pr(pr[1], model, get_pr)

  ar_t = c()
  for(i in 1:length(pr)){
    ar = dts_pr2ar_step(pr[i], model, get_pr, 1, tol)
    y = update_by_ar(ar, model, 1)
    model = update_inits(y, model)
    ar_t = c(ar_t, ar)
  }

  return(ar_t)
}
