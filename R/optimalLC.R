#' Optimiser to maximize Expected Utility
#'
#' Here we minimize negative expected utility (given by function `util`). Parameters can be fixed by setting "tight" boundaries.
#'
#' @param initial_values Starting values c(ret_age,c, c2, nu2, nu3, alpha, w3)
#' @param upper_bounds Upper bounds for optimization
#' @param lower_bounds Lower bounds for optimization
#' @param ... all other parameters
#'
#' @return Expected utility
#'
#' @examples
#' \dontrun{
# data(ret);data(retr)
# .load_parameters()
# initial_values <- c(65, 0.6, 0.12, 0.5, 0.2, 0.96, 0.25, 0.25, 0.25)
# upper_bounds <- c(66, 1, 0.14, 1, 1, 1, 1, 1, 1)
# lower_bounds <- c(64, 0, 0.11, 0, 0, 0, 0, 0, 0)
# optimalLC(initial_values,upper_bounds,lower_bounds,ra=ra,delta=delta,beta=beta,c_age=c_age,gender=gender,gender_mortalityTable=gender_mortalityTable,
#          w0=w0,CF=CF,li=li,lg=lg,c1=c1,s1=s1,s2=s2,s3=s3,w2=w2,rho2=rho2,rho3=rho3,ret=ret[,,1:10],retr=retr[,,1:10],psi=psi,trace=0)
#' }
#' @importFrom optimx optimx
#'
#' @export
optimalLC <- function(initial_values,upper_bounds,lower_bounds,ra,delta,beta,c_age,gender,gender_mortalityTable,w0,CF,li,lg,c1,s1,s2,s3,w2,rho2,rho3,ret,retr,psi,trace){
  res <- opm(initial_values,
             fn=.util_optim,#gr=function(x) pracma::gradient(util_optim,x),
             #method = c('Nelder-Mead','L-BFGS-B'),
             #lower=lower_bounds,
             #upper=upper_bounds,
             method=c("Nelder-Mead","L-BFGS-B","ucminf","nmk"),
             control=list(#all.methods=TRUE,
             save.failures=TRUE, maxit=50, trace=trace),#, factr = 1e-10),
             ra=ra,delta=delta,beta=beta,c_age=c_age,gender=gender,
             gender_mortalityTable=gender_mortalityTable,
             w0=w0,li=li,lg=lg,c1=c1,s1=s1,s2=s2,s3=s3,w2=w2,
             rho2=rho2,rho3=rho3,
             ret=ret,retr=retr,psi=psi,verbose=verbose)
  return(res)
}
