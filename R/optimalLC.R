#' Optimizer to maximize Expected Utility
#'
#' Here we minimize negative expected utility (given by function `util()`). Parameters can be fixed by setting "tight" boundaries
#'
#' @param initial_values Starting values c(ret_age, c, c2, nu2, nu3, alpha, w3)
#' @param upper_bounds Upper bounds for optimization
#' @param lower_bounds Lower bounds for optimization
#' @param c_age Given variable: the investor's current age (assuming birthday is calculation-day)
#' @param gender Given variable: gender, 0=male and 1=female
#' @param w0 Given variable: time c_age wealth that is not disposable, assumption: still available at retirement (no growth or decline),
#' alternatively: expected wealth (that is not disposable) at retirement, stays the same over time
#' @param CF Given Variables: income shocks, such as inheritance (not currently implemented)
#' @param li Given variable: gross labor income at time 0 (in the last year before birthday)
#' @param lg Given variable: labor growth rate (in real terms, constant)
#' @param c1 Given variable: first pillar savings as fraction of gross income
#' @param s1 Given variable: vector consisting of two components: c(number of contribution years at age=c_age,historical average yearly income until c_age)
#' @param s2 Given variable: savings in second pillar as of t=0
#' @param s3 Given variable: liquid wealth - invested in the third pillar (current assumption: no tax advantage for third pillar)
#' @param w2 Given variable: portfolio allocation in second pillar (assumed to be fixed and not influenced by the decision maker)
#' @param rho2 Given variable: conversion factor in second pillar for regular retirement age
#' @param rho3 Given variable: conversion factor in third pillar for regular retirement age
#' @param ret Given variable: investment return scenarios (nominal)
#' @param retr Given variable: investment return scenarios (real)
#' @param psi Given variable: optional, spread to take a loan/leverage for third pillar savings
#' @param warnings optional: should warnings be given? (default=TRUE)
#'
#' @return Optimal values for c(ret_age, c, c2, nu2, nu3, alpha, w3)
#'
#' @examples
#' \dontrun{
#' data(ret);data(retr)
#' .load_parameters()
#' initial_values <- c(0.6, 0.12, 0.5, 0.2, 0.96, 0.25, 0.25, 0.25)
#' optimalLC(initial_values,upper_bounds=NULL,lower_bounds=NULL,
#' ret_age=ret_age,ra=ra,delta=delta,beta=beta,c_age=c_age,
#' gender=gender,gender_mortalityTable=gender_mortalityTable,
#'          w0=w0,CF=CF,li=li,lg=lg,c1=c1,s1=s1,s2=s2,s3=s3,
#'          w2=w2,rho2=rho2,rho3=rho3,ret=ret[,,1:10],
#'          retr=retr[,,1:10],psi=psi,trace=1,reltol=1e-5)
#'
#' }
#' @importFrom optimx optimx
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#'
#' @export
optimalLC <- function(initial_values,upper_bounds,lower_bounds,ret_age,ra,delta,beta,c_age,gender,gender_mortalityTable,w0,
                      CF,li,lg,c1,s1,s2,s3,w2,rho2,rho3,ret,retr,psi,verbose=FALSE,warnings=FALSE,trace=0,reltol=sqrt(.Machine$double.eps)){
  #mc <- data.frame(method=c("Nelder-Mead","L-BFGS-B"), maxit=c(50,50), maxfeval= c(50,50))
  mc <- data.frame(method=c("Nelder-Mead"), maxit=c(50*9^2), maxfeval= c(50*9^2), reltol=sqrt(.Machine$double.eps)*100)
  ivmat <- rbind(initial_values,
                 as.matrix(rbind(c(0.5,0.1,0,0,1,rep(1/3,3)),
                           c(0.5,0.1,0.1,0.1,1,rep(1/3,3)))))
  res <- NULL
  for (i in c(1:3)){
    resn <- optim(par=ivmat[i,],
                            fn=.util_optim, #gr=NULL,hess=NULL,#gr=function(x) pracma::gradient(util_optim,x),
                            #lower=lower_bounds,
                            #upper=upper_bounds,
                            #methcontrol=mc,
                            method="Nelder-Mead",
                            control=list(#all.methods=TRUE,
                              reltol=reltol,
                              trace=trace,
                              maxit=50*9^2),#, factr = 1e-10),
                            #itnmax=50*3^2,
                            ret_age=ret_age,
                            ra=ra,delta=delta,beta=beta,c_age=c_age,gender=gender,
                            gender_mortalityTable=gender_mortalityTable,
                            w0=w0,CF=CF,li=li,lg=lg,c1=c1,s1=s1,s2=s2,s3=s3,w2=w2,
                            rho2=rho2,rho3=rho3,
                            ret=ret,retr=retr,psi=psi,verbose=verbose,warnings=warnings)
    res <- tibble::tibble("i"=1,"method"=c("Nelder-Mead"),dplyr::bind_rows(unlist(resn)))
    cat("Round ",i,"\n")
    if (min(res["convergence"])==0) {break()}
  }
  return(res)
}
