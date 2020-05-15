#' Optimiser to maximize Expected Utility
#'
#' Here we minimize negative expected utility (given by function `util`). Parameters can be fixed by setting "tight" boundaries
#'
#' @param initial_values Starting values c(ret_age, c, c2, nu2, nu3, alpha, w3)
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
# initial_values <- c(0.6, 0.12, 0.5, 0.2, 0.96, 0.25, 0.25, 0.25)
# upper_bounds <- c(1, 0.14, 1, 1, 1, 1, 1, 1)
# lower_bounds <- c(0, 0.11, 0, 0, 0, 0, 0, 0)
# optimalLC(initial_values,upper_bounds,lower_bounds,ra=ra,delta=delta,beta=beta,c_age=c_age,gender=gender,gender_mortalityTable=gender_mortalityTable,
#          w0=w0,CF=CF,li=li,lg=lg,c1=c1,s1=s1,s2=s2,s3=s3,w2=w2,rho2=rho2,rho3=rho3,ret=ret[,,1:10],retr=retr[,,1:10],psi=psi,trace=0)
#' }
#' @importFrom optimx optimx
#'
#' @export
optimalLC <- function(initial_values,upper_bounds,lower_bounds,ra,delta,beta,c_age,gender,gender_mortalityTable,w0,
                      CF,li,lg,c1,s1,s2,s3,w2,rho2,rho3,ret,retr,psi,verbose=FALSE,warnings=FALSE,trace){
  #mc <- data.frame(method=c("Nelder-Mead","L-BFGS-B"), maxit=c(50,50), maxfeval= c(50,50))
  mc <- data.frame(method=c("Nelder-Mead"), maxit=c(50*9^2), maxfeval= c(50*9^2))
  ivmat <- as.matrix(rbind(c(0.5,0.1,0,0,1,rep(1/3,3),65),
                           c(0.5,0.1,0.1,0.1,1,rep(1/3,3),65)))
  res <- NULL
  for (i in c(1:3)){
    resn <- optimx::polyopt(par=initial_values,
                            fn=.util_optim, gr=NULL,hess=NULL,#gr=function(x) pracma::gradient(util_optim,x),
                            #lower=lower_bounds,
                            #upper=upper_bounds,
                            methcontrol=mc,
                            control=list(#all.methods=TRUE,
                              #save.failures=TRUE, maxit=50*5,
                              trace=trace),#, factr = 1e-10),
                            #itnmax=50*3^2,
                            #ret_age=ret_age,
                            ra=ra,delta=delta,beta=beta,c_age=c_age,gender=gender,
                            gender_mortalityTable=gender_mortalityTable,
                            w0=w0,CF=CF,li=li,lg=lg,c1=c1,s1=s1,s2=s2,s3=s3,w2=w2,
                            rho2=rho2,rho3=rho3,
                            ret=ret,retr=retr,psi=psi,verbose=verbose,warnings=warnings)
    res <- cbind("i"=1,"method"=c("Nelder-Mead"),data.frame(resn, row.names=NULL))
    cat("Round ",i,"\n")
    if (min(res["convergence"])==0) {break()}
    initial_values <- ivmat[i,]
  }
  return(res)
}
