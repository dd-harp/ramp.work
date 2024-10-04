#' @title sum of squared errors
#' @description Compute the sum of squared errors
#' @param ppp the parameters to be fitted
#' @param data a set of observations
#' @param model a model
#' @param F_obs a function to return the predicted observations from the model
#' @param put_par a function to put the parameters to be fitted
#' @param F_gof a function to compute the goodness of fit
#' @param Tmax the maximum runtime
#' @return the sum of squared errors
#' @export
xde_compute_gof = function(ppp, data, model, F_obs, put_par, F_gof,  Tmax=3650){
   model <- put_par(ppp, model)
   model <- xds_solve(model, Tmax, Tmax)
   pred <- F_obs(model)
   model <- last_to_inits(model)
   gof <- F_gof(data, pred)
   return(gof)
}

#' @title sum of squared errors
#' @description Compute the sum of squared errors
#' @param data a set of observations
#' @param model a model
#' @param F_obs a function to return the predicted observations from the model
#' @param get_par a function to get the parameters to be fitted
#' @param put_par a function to put the parameters to be fitted
#' @param F_gof a function to compute the goodness of fit
#' @param Tmax the maximum runtime
#' @param tol the desired accuracy
#' @return the sum of squared errors
#' @export
xde_maximize_gof = function(data, model, F_obs, get_par, put_par, F_gof, Tmax=3650, tol = 1e-8){
  xxx = 1
  while(xxx > tol){
    ppp <- get_par(model)
    if(length(ppp)==1){
      vals <- stats::optimize(xde_compute_gof, interval = c(1, 5*ppp), data=data, model=model, F_obs=F_obs, put_par=put_par, F_gof=F_gof, Tmax=Tmax)
    } else {
      vals <- stats::nlm(xde_compute_gof, ppp, data=data, model=model, F_obs=F_obs, put_par=put_par, F_gof=F_gof, Tmax=Tmax)
    }
    model <- put_par(vals$minimum, model)
    model <- xds_solve(model, 3*Tmax)
    xxx <- F_gof(data, F_obs(model))
    model <- last_to_inits(model)
  }
  return(model)
}

#' @title sum of squared errors
#' @description Compute the sum of squared errors
#' @param ppp the parameters to be fitted
#' @param data a set of observations
#' @param model a model
#' @param F_obs a function to return the predicted observations from the model
#' @param put_par a function to put the parameters to be fitted
#' @param F_gof a function to compute the goodness of fit
#' @param Tmax the maximum runtime
#' @return the sum of squared errors
#' @export
dts_compute_gof = function(ppp, data, model, F_obs, put_par, F_gof, Tmax=365){
  model <- put_par(ppp, model)
  model <- xds_solve(model, Tmax)
  pred <- F_obs(model)
  model <- last_to_inits(model)
  gof <- F_gof(data, pred)
  return(gof)
  #  F_gof(data, F_obs(xds_solve(put_par(ppp, model), Tmax, dt)))
}

#' @title sum of squared errors
#' @description Compute the sum of squared errors
#' @param data a set of observations
#' @param model a model
#' @param F_obs a function to return the predicted observations from the model
#' @param get_par a function to get the parameters to be fitted
#' @param put_par a function to put the parameters to be fitted
#' @param F_gof a function to compute the goodness of fit
#' @param Tmax the maximum runtime
#' @param dt the time step
#' @param tol the desired accuracy
#' @return the sum of squared errors
#' @export
dts_maximize_gof = function(data, model, F_obs, get_par, put_par, F_gof, Tmax=365, dt=1, tol = 1e-8){
  xxx = 1
  while(xxx > tol){
    ppp <- get_par(model)
    if(length(ppp)==1){
      vals <- stats::optimize(dts_compute_gof, interval=c(1/5*ppp, 5*ppp), data=data, model=model, F_obs=F_obs, put_par=put_par, F_gof=F_gof, Tmax=Tmax, dt=dt)
    }else{
      vals <- stats::nlm(dts_compute_gof, ppp, data=data, model=model, F_obs=F_obs, put_par=put_par, F_gof=F_gof, Tmax=Tmax, dt=dt)
    }
    model <- put_par(vals$minimum, model)
    model <- xds_solve(model, 3*Tmax)
    xxx <- F_gof(data, F_obs(model))
    model <- last_to_inits(model)
  }
  return(model)
}

#' @title Sum of squared errors. Works as F_gof in xde_maximize_gof
#' @description Compute the sum of squared errors
#' @param obs a set of observations
#' @param pred a set of predicted values for the observations
#' @return the sum of squared errors
#' @export
F_sse = function(obs, pred){
  sum((obs-pred)^2)
}



