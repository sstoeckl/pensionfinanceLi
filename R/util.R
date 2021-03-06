#' Calculate utility of total consumption and bequest
#'
#' Expected Utility for total cash-flows is calculated here and used for optimization
#'
#' @param ret_age Decision Variable: retirement age, can be set anywhere between 60 and 70 (default: 65)
#' @param tw3 Decision Variable: third pillar portfolio allocation (given either as vector or as matrix with entries) for all years.
#' HERE: Choose only allocation to stocks, bonds and real estate, cash will be determined as fraction missing to sum up to one
#' @param c Decision Variable: fraction of income that is consumed while still working (current assumption: constant)
#' @param c2 Decision Variable: second pillar savings as fraction of gross income (still missing: health, a-fonds-perdu payments)
#' @param nu2 Decision Variable: fraction of second pillar savings that is converted to life-long pension
#' @param nu3 Decision Variable: fraction of third pillar savings that is converted to life-long pension
#' @param alpha Decision Variable: parameter to choose fraction of wealth NOT consumed during retirement but kept for investment (and subsequent consumption) - see file 'Consumption_3p.ods'
#' @param beta Given variable: Relative Weight of bequest utility
#' @param ra Given variable: Risk Aversion of Agent
#' @param delta Given Variable: Time Preference
#' @param c_age Given variable: the investor's current age (assuming birthday is calculation-day)
#' @param gender Given variable: gender, 0=male and 1=female
#' @param gender_mortalityTable Given variable: MortalityTable to use for gender 0=male and 1=female, e.g. `baseTable(AVOe2005R.male)`
#' @param w0 Given variable: time c_age wealth that is not disposable, assumption: still available at retirement (no growth or decline),
#' alternatively: expected wealth (that is not disposable) at retirement, stays the same over time
#' @param CF Given Variables: income shocks, such as inheritance (not currrently imlemented)
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
#' @param psi Given variable: spread to take a loan/leverage for third pillar savings
#' @param verbose optional: show additional information while calculating utility (default: FALSE)
#' @param warnings optional: should warnings be given? (default=TRUE)
#'
#' @return Expected utility
#'
#' @examples
#' data(ret); data(retr)
#' MortalityTables::mortalityTables.load("Austria_Annuities")
#'
#' util_ex <- util(ret_age=65,c_age=42,
#'                 tw3=c(.25,.25,.25),
#'                 c=0.6,c2=.12,nu2=.5,nu3=0.01,ra=4,delta=0.02,alpha=0.96,beta=0.75,gender=0,
#'                 gender_mortalityTable=MortalityTables::baseTable(AVOe2005R.male),
#'                 w0=300000,CF=NULL,li=100000,lg=0.01,c1=0.07,s1=c(15,80000),s2=300000,s3=300000,
#'                 w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
#'                 rho2=0.05,rho3=0.04,ret=ret,retr=retr,psi=0.015)
#' util_ex2 <- util(ret_age=65,c_age=64,
#'                 tw3=setNames(c(1,0,0),c("msci","b10","recom")),
#'                 c=1,c2=0,nu2=0,nu3=0,ra=0,delta=0,alpha=1,beta=0,gender=0,
#'                 gender_mortalityTable=MortalityTables::baseTable(AVOe2005R.male),
#'                 w0=0,CF=NULL,li=0,lg=0,c1=0,s1=c(0,0),s2=0,s3=0,
#'                 w2=setNames(c(0,0,0,1,0),c("msci","b10","recom","libor","infl")),
#'                 rho2=0.0001,rho3=0.0001,ret=ret[,,1:10],retr=retr[,,1:10],psi=0)
#'
#' @importFrom stats setNames
#'
#' @export
util <- function(ret_age,tw3,c,c2,nu2,nu3,ra,delta,alpha,beta,c_age,gender,gender_mortalityTable,w0,CF,li,lg,c1,s1,s2,s3,w2,rho2,rho3,
                 ret,retr,psi,verbose=FALSE, warnings=TRUE){
  #### PARAMETER CHECK: If not fulfilled, then punish utility
  if (any(tw3<0)|sum(tw3)>1.5|((s3<0)&(alpha>1))|nu2<0|nu2>1|nu3>1|nu3<0|ret_age<60|ret_age>70|c>1|c<0|c2>1|c2<0){
    EU <- -5000
  } else {
    w3 <- setNames(rep(0,5),c("msci","b10","recom","libor","infl"))
    w3[c("msci","b10","recom")] <- tw3
    w3["libor"] <- if (!abs(sum(tw3))==Inf) {1 - sum(tw3)}
    #w3["infl"] <- 0
    if (verbose) print(w3)
    # warnings
    if (warnings){
      if (ra==1) warning ("Using log utility rather than power utility")
    }
    #########################################
    ## 1. Calculate total cash-flow
    tcf <- totalCF(ret_age = ret_age, w3 = w3, c = c, c2 = c2,
                   nu2 = nu2, nu3 = nu3, c_age = c_age, gender = gender,
                   w0 = w0, li = li, lg = lg, c1 = c1, s1 = s1, s2 = s2, s3 = s3,
                   w2 = w2, rho2 = rho2, rho3 = rho3, ret = ret, retr = retr, psi = psi, alpha = alpha, warnings=warnings)
    ## stop if any total consumption term is negative
    if (min(tcf$cons)<0){
      EU <- -5000
    } else if (- min(tcf$wealth_after_ret,tcf$wealth_before_ret) > 0.8 * w0) {
      EU <- -5000
    } else {
      #########################################
      ## 2. prepare discount factors and survival probabilities
      delta_vec <- (1-delta)^(seq(1,(122-c_age+1)))
        names(delta_vec) <- (c_age):122
      # cumulative survival probabilities
      sur <- cumprod(1-gender_mortalityTable[(c_age-1):(length(gender_mortalityTable)-1)])
      #########################################
      ## 3. Calculate utilities
      # find those that have negative cons or wealth
      wealth <- rbind(tcf$wealth_before_ret,tcf$wealth_after_ret)
      uncondmort <- c(1,sur)*gender_mortalityTable[(c_age-1):length(gender_mortalityTable)]
      uncondmort <- uncondmort[-length(uncondmort)]
      #### adapt cons and wealth to have -Inf wherever <0 in cons or wealth (therefore it does count for the utility function but scales it down)
      if (ra==1){
        # del_index <- union(which(apply(tcf$cons,2,function(x) max(x<=0))==1),which(apply(wealth,2,function(x) max(x<=0))==1))
        # tcf$cons[,del_index] <- 0.5
        # wealth[,del_index] <- 0.5
        ### 3a. utility of entire lifetime consumption (scaled for numerical reasons)
        UC <- apply(tcf$cons,2,function(x){sum(log(x)*delta_vec*sur)})
        ### 3b. utility of bequest
        UB <- apply(wealth,2,function(x){sum(log(x)*delta_vec*uncondmort)})
      } else {
        # del_index <- union(which(apply(tcf$cons,2,function(x) max(x<=0))==1),which(apply(wealth,2,function(x) max(x<=0))==1))
        # tcf$cons[,del_index] <- 0
        # wealth[,del_index] <- 0
        ### 3a. utility of entire lifetime consumption (scaled for numerical reasons)
        UC <- apply(tcf$cons,2,function(x){sum((((x+max(10000,w0))/max(10000,w0))^(1-ra))/(1-ra)%*%delta_vec*sur)})
        ### 3b. utility of bequest
        UB <- apply(wealth,2,function(x){sum((((x+max(10000,w0))/max(10000,w0))^(1-ra))/(1-ra)%*%delta_vec*uncondmort)})
      }
        ### 3c. Expected utility
        EU <- mean(UC+beta*UB)
    }
  }
  return(EU)
}
#' Helper functions
#'
#' Helper 1: Optimization function
#'
#' @param inputvec c(c, c2, nu2, nu3, alpha, w3)
#'
.util_optim <- function(inputvec,ret_age,ra,delta,beta,c_age,gender,gender_mortalityTable,w0,CF,li,lg,c1,s1,s2,s3,w2,rho2,rho3,ret,retr,psi,verbose=FALSE, warnings=FALSE){
  #ret_age,tw3,c,c2,nu2,nu3,delta,alpha
  # inputvec=c(ret_age,c,c2,nu2,nu3,alpha,tw3)
  return(-util(ret_age=ret_age,tw3=inputvec[6:8],c=inputvec[1],c2=inputvec[2],nu2=inputvec[3],nu3=inputvec[4],ra=ra,delta=delta,alpha=inputvec[5],
              beta=beta,c_age=c_age,gender=gender,
              gender_mortalityTable=gender_mortalityTable,w0=w0,CF=CF,li=li,lg=lg,c1=c1,s1=s1,s2=s2,s3=s3,w2=w2,rho2=rho2,rho3=rho3,
              ret=ret,retr=retr,psi=psi,verbose=verbose, warnings=warnings))
}
#' Helper 2: Optimize portfolio weights and consumption c,
#'
#' @param inputvec c(c, alpha, w3)
#'
.util_optim_wc <- function(inputvec,ret_age,c2,nu2,nu3,ra,delta,beta,c_age,gender,gender_mortalityTable,w0,CF,li,lg,c1,s1,s2,s3,w2,rho2,rho3,ret,retr,psi,verbose=FALSE, warnings=FALSE){
  #ret_age,tw3,c,c2,nu2,nu3,delta,alpha
  # inputvec=c(ret_age,c,c2,nu2,nu3,alpha,tw3)
  return(-util(ret_age=ret_age,tw3=inputvec[3:5],c=inputvec[1],c2=c2,nu2=nu2,nu3=nu3,ra=ra,delta=delta,alpha=inputvec[2],
               beta=beta,c_age=c_age,gender=gender,
               gender_mortalityTable=gender_mortalityTable,w0=w0,CF=CF,li=li,lg=lg,c1=c1,s1=s1,s2=s2,s3=s3,w2=w2,rho2=rho2,rho3=rho3,
               ret=ret,retr=retr,psi=psi,verbose=verbose, warnings=warnings))
}
#' Helper 3: Optimize only portfolio weights
#'
#' @param inputvec c(w3)
#'
.util_optim_w <- function(inputvec,ret_age,cc,c2,nu2,nu3,ra,delta,alpha,beta,c_age,gender,gender_mortalityTable,w0,CF,li,lg,c1,s1,s2,s3,w2,rho2,rho3,ret,retr,psi,verbose=FALSE, warnings=FALSE){
  #ret_age,tw3,c,c2,nu2,nu3,delta,alpha
  # inputvec=c(ret_age,c,c2,nu2,nu3,alpha,tw3)
  return(-util(ret_age=ret_age,tw3=inputvec,c=cc,c2=c2,nu2=nu2,nu3=nu3,ra=ra,delta=delta,alpha=alpha,
              beta=beta,c_age=c_age,gender=gender,
              gender_mortalityTable=gender_mortalityTable,w0=w0,CF=CF,li=li,lg=lg,c1=c1,s1=s1,s2=s2,s3=s3,w2=w2,rho2=rho2,rho3=rho3,
              ret=ret,retr=retr,psi=psi,verbose=verbose, warnings=warnings))
}
