#' Setup Imputation
#'
#' @description This sets up the the imputatation model object. It
#' sets options for using interpolation points to impute a baseline
#' or to hindcast or forecast.
#'
#' @param impute_ty a text string to dispatch `impute_baseline_ty`
#' @param trusted_ty a text string to dispatch `get_trusted_ty`
#'
#' @returns an imputation model object
#'
#'
#' @export
setup_imputation = function(impute_ty = "mean", trusted_ty="unmodified"){

  impute_obj <- list()

  class(impute_ty) = impute_ty
  impute_obj$impute_ty = impute_ty

  class(trusted_ty) = trusted_ty
  impute_obj$trusted_ty = trusted_ty

  return(impute_obj)
}
