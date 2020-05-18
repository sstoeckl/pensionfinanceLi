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
#' outwc <- optimalwc(initial_values,upper_bounds=NULL,lower_bounds=NULL,ret_age=ret_age,c2=c2,nu2=nu2,nu3=nu3,
#'          ra=18,delta=delta,beta=bbeta,c_age=c_age,gender=gender,gender_mortalityTable=gender_mortalityTable,
#'          w0=w0,CF=CF,li=li,lg=lg,c1=c1,s1=s1,s2=s2,s3=s3,w2=w2,rho2=rho2,rho3=rho3,ret=ret[,,1:10],retr=retr[,,1:10],psi=psi,trace=1,reltol=1e-4)
#'
#' @importFrom optimx optimx
#' @importFrom tibble tibble
#'
#' @export
optimalwc <- function(initial_values,upper_bounds,lower_bounds,ret_age,c2,nu2,nu3,ra,delta,beta,c_age,gender,gender_mortalityTable,w0,
                      CF,li,lg,c1,s1,s2,s3,w2,rho2,rho3,ret,retr,psi,verbose=FALSE,warnings=FALSE,trace=0,reltol=sqrt(.Machine$double.eps)){
  #mc <- data.frame(method=c("Nelder-Mead","L-BFGS-B"), maxit=c(50,50), maxfeval= c(50,50))
  mc <- data.frame(method=c("Nelder-Mead"), maxit=c(50*5^2), maxfeval= c(50*5^2))
  ivmat <- rbind(initial_values,as.matrix(rbind(c(0.5,1,rep(0,3)),c(1,1,rep(0,3)))))
  res <- NULL
  for (i in c(1:3)){
    resn <- optim(par=ivmat[i,],
                fn=.util_optim_wc, #gr=NULL,hess=NULL,#gr=function(x) pracma::gradient(util_optim,x),
                #lower=lower_bounds,
                #upper=upper_bounds,
                method="Nelder-Mead",
                control=list(#all.methods=TRUE,
                  reltol=reltol,
                  trace=trace,
                  maxit=50*5^2),#, factr = 1e-10),
                #itnmax=50*3^2,
                ret_age=ret_age,c2=c2,nu2=nu2,nu3=nu3,
                ra=ra,delta=delta,beta=beta,c_age=c_age,gender=gender,
                gender_mortalityTable=gender_mortalityTable,
                w0=w0,CF=CF,li=li,lg=lg,c1=c1,s1=s1,s2=s2,s3=s3,w2=w2,
                rho2=rho2,rho3=rho3,
                ret=ret,retr=retr,psi=psi,verbose=verbose,warnings=warnings)
    res <- tibble::tibble("i"=1,"method"=c("Nelder-Mead"),bind_rows(unlist(resn)))
    cat("Round ",i,"\n")
    if (min(res["convergence"])==0) {break()}
  }
  return(res)
}
