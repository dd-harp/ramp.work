#' @title Create a cohort matrix
#' @description Return a matrix `dH`
#' @param deathrates the death rates for
#' @param ageMesh a partition on human age
#' @return the steady states as a named vector
#' @export
UnevenAgingMatrix = function(deathrates, ageMesh){
  N = length(ageMesh)
  checkIt(deathrates, N+1)
  ageUp = c(1/ageMesh,0)
  Aging = diag(-ageUp-deathrates)
  Aging[cbind(1+1:N, 1:N)] = 1/ageMesh
  return(Aging)
}

