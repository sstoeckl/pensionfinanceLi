#' Optimiser to maximize Expected Utility
#'
#' Here we minimize negative expected utility (given by function `util`). Parameters can be fixed by setting "tight" boundaries
#'
#' @param initial_values Starting values c(c, alpha, w3)
#' @param upper_bounds Upper bounds for optimization
#' @param lower_bounds Lower bounds for optimization
#' @param ... all other parameters
#'
#' @return Expected utility
#'
#' @examples
#'
#' data(ret);data(retr)
#' .load_parameters()
#' initial_values <- c(0.5, 0.95, 0.25, 0.25, 0.25)
#' upper_bounds <- c(1, 1, 2, 2, 2)
#' lower_bounds <- c(0, 0, -2, -2, -2)
#' outwc <- optimalwc(initial_values,upper_bounds,lower_bounds,ret_age=ret_age,c2=c2,nu2=nu2,nu3=nu3,
#'          ra=18,delta=delta,beta=beta,c_age=c_age,gender=gender,gender_mortalityTable=gender_mortalityTable,
#'          w0=w0,CF=CF,li=li,lg=lg,c1=c1,s1=s1,s2=s2,s3=s3,w2=w2,rho2=rho2,rho3=rho3,ret=ret[,,1:10],retr=retr[,,1:10],psi=psi,trace=0)
#'
#' @importFrom optimx optimx
#'
#' @export
optimalwc <- function(initial_values,upper_bounds,lower_bounds,ret_age,c2,nu2,nu3,ra,delta,beta,c_age,gender,gender_mortalityTable,w0,
                      CF,li,lg,c1,s1,s2,s3,w2,rho2,rho3,ret,retr,psi,verbose=FALSE,warnings=FALSE,trace=0){
  #mc <- data.frame(method=c("Nelder-Mead","L-BFGS-B"), maxit=c(50,50), maxfeval= c(50,50))
  mc <- data.frame(method=c("Nelder-Mead"), maxit=c(50*5^2), maxfeval= c(50*5^2))
  ivmat <- as.matrix(rbind(c(0.5,1,rep(0,3)),c(0.5,1,rep(1,3)),c(0.5,1,-1,0,1),c(0.5,1,0,1,0),c(0.5,1,0,0,1)))
  res <- NULL
  for (i in c(1:1)){
    resn <- optimx::polyopt(par=initial_values,
                fn=.util_optim_wc, gr=NULL,hess=NULL,#gr=function(x) pracma::gradient(util_optim,x),
                #lower=lower_bounds,
                #upper=upper_bounds,
                methcontrol=mc,
                control=list(#all.methods=TRUE,
                #save.failures=TRUE, maxit=50*5,
                trace=trace),#, factr = 1e-10),
                #itnmax=50*3^2,
                ret_age=ret_age,c2=c2,nu2=nu2,nu3=nu3,
                ra=ra,delta=delta,beta=beta,c_age=c_age,gender=gender,
                gender_mortalityTable=gender_mortalityTable,
                w0=w0,CF=CF,li=li,lg=lg,c1=c1,s1=s1,s2=s2,s3=s3,w2=w2,
                rho2=rho2,rho3=rho3,
                ret=ret,retr=retr,psi=psi,verbose=verbose,warnings=warnings)
    res <- cbind("i"=1,"method"=c("Nelder-Mead"),data.frame(resn, row.names=NULL))
    cat("Round ",i,"\n")
    if (min(res[-1,"convergence"])==0) {break()}
    initial_values <- ivmat[i,]
  }
  return(res)
}
