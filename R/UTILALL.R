#' Calculate utility of total consumption and bequest (FAST VERSION)
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
#' @param alpha parameter to choose fraction of wealth NOT consumed during retirement but kept for investment (and subsequent consumption) - see file 'Consumption_3p.ods'
#' @param beta Given variable: Relative Weight of bequest utility
#' @param ra Given variable: Risk Aversion of Agent
#' @param delta Given Variable: Time Preference
#' @param c_age Given variable: the investor's current age (assuming birthday is calculation-day)
#' @param gender Given variable: gender, 0=male and 1=female
#' @param gender_mortalityTable2 Given variable: Combined MortalityTable with columns for both gender 0=male and 1=female, e.g. `baseTable(AVOe2005R.male)`
#' @param w0 Given variable: time c_age wealth that is not disposable, assumption: still available at retirement (no growth or decline),
#' alternatively: expected wealth (that is not disposable) at retirement, stays the same over time
#' @param CF Given Variables: income shocks, such as inheritance (not currrently imlemented)
#' @param li Given variable: gross labor income at time 0 (in the last year before birthday)
#' @param lg Given variable: labor growth rate (in real terms, constant)
#' @param c1 Given variable: first pillar savings as fraction of gross income
#' @param s1 Given variable: vector consisting of two components: c(number of contribution years at age=c_age,historical average yearly income until c_age)
#' @param s2 Given variable: savings in second pillar as of t=0
#' @param s3 Given variable: liquid wealth - invested in the third pillar (current assumption: no tax advantage for third pillar)
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
#' data(ret); data(retr); data(SPFret)
#' MortalityTables::mortalityTables.load("Austria_Annuities")
#' .load_parameters(gend=0,type=1)
#' SPFretsel <- .SPFretch(SPFret,c_age=c_age,ret_age=ret_age)
#'
#' utilall_ex <- utilall(ret_age=ret_age,c_age=c_age,
#'                 tw3=c(.25,.25,.25),
#'                 c=cc,c2=c2,nu2=nu2,nu3=nu3,ra=ra,delta=delta,alpha=aalpha,beta=bbeta,gender=gender,
#'                 gender_mortalityTable2=cbind(MortalityTables::baseTable(AVOe2005R.male),MortalityTables::baseTable(AVOe2005R.female)),
#'                 w0=w0,CF=NULL,li=li,lg=lg,c1=c1,s1=s1,s2=s2,s3=s3,
#'                 rho2=rho2,rho3=rho3,ret=ret,retr=retr,SPFretsel=SPFretsel,psi=psi)
#'
#' .load_parameters(gend=1,type=2)
#' SPFretsel <- .SPFretch(SPFret,c_age=c_age,ret_age=ret_age)
#' utilall_ex2 <- utilall(ret_age=ret_age,c_age=c_age,
#'                 tw3=c(.25,.25,.25),
#'                 c=cc,c2=c2,nu2=nu2,nu3=nu3,ra=ra,delta=delta,alpha=aalpha,beta=bbeta,gender=gender,
#'                 gender_mortalityTable2=cbind(MortalityTables::baseTable(AVOe2005R.male),MortalityTables::baseTable(AVOe2005R.female)),
#'                 w0=w0,CF=NULL,li=li,lg=lg,c1=c1,s1=s1,s2=s2,s3=s3,
#'                 rho2=rho2,rho3=rho3,ret=ret,retr=retr,SPFretsel=SPFretsel,psi=psi)
#'
#' @importFrom stats setNames
#'
#' @export
utilall <- function(ret_age,tw3,c,c2,nu2,nu3,ra,delta,alpha,beta,c_age,gender,gender_mortalityTable2,w0,CF,li,lg,c1,s1,s2,s3,rho2,rho3,
                    ret,retr,SPFretsel,psi,verbose=FALSE, warnings=TRUE){
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
    #########################################
    ## 1. Pre-Calculations
    # years left for saving
    sav_years <- ret_age - c_age
    #########################################
      ## 2. Cash-Flows
        ### 2a. First Pillar
        # generate vector of labor income for remaining working years
        #laborincome <- li*(1+lg)^(seq(sav_years))
        # calculate average of labor income in remaining working years
        #avg_laborincome <- mean(li*(1+lg)^(seq(sav_years)))
        # weigh with average labor income before today
        # 2.1 is a "mysterious revaluation factor" used by AHV
        avg_laborincome <- (mean(li*(1+lg)^(seq(sav_years)))*sav_years + s1[1]*s1[2])/(sav_years + s1[1])*2.1
        # Data
        pension_adj <- setNames(c(-0.218,-0.18,-0.14,-0.097,-0.05,0,0.045,0.093,0.144,0.201,0.261),60:70)
        #########################################
        ## 2. First-Pillar specifics
        # Adjust income that will be evalued by 1stP to lie within these brackets:
        avg_laborincome[avg_laborincome<13920] <- 13920
        avg_laborincome[avg_laborincome>83520] <- 83520
        # The following calculates the theoretical pension to be received given 44 (max) contribution years, it is an approximation based on an (?) Excel regression
        full_pension <- (815.4 + 0.02622247*avg_laborincome - 1.0193*10^(-7)*avg_laborincome^2)*13 # from regression in excel to replace AHV table
        # Adjust for fraction of contribution years (base and max: 44 years)
        # yearfrac <- min((s1[1]+sav_years)/44,1) #assumption that 44 contribution years
        # calculate actual 1stP pension at ret_age and adjust for late/early retirement
        fp_pensionstart <- min((s1[1]+sav_years)/44,1)*full_pension*(1+pension_adj[as.character(ret_age)])
        #########################################
        ## 3. Adjustment for inflation
        # adjustment for inflation: simplifying assumption of "always half of observed inflation rate" (even if inflation is negative)
        # adjustment for only half the inflation leads to deflation in real terms
        #h2 <- apply(ret[ret_age:122,"infl",],2,function(x) exp(-cumsum(x/2)))
        #rownames(h2) <- as.character(ret_age:122)
        # Now create final vector of 1st pillar pension cashflows
        fpcf <- drop(fp_pensionstart)*exp(-apply(ret[ret_age:122,"infl",]/2,2,function(x) cumsum(x)))
      ### 2b. Second Pillar
        #########################################
        ## 3. Cashflow
        # savings in second pillar (starting with savings until now, growing on with fraction of (growing) labor income * 2)
        lcf <- c(s2,(c2+min(c2,0.12))*li*(1+lg)^(seq(sav_years)-1))  # 2*c2 because of 50/50 contribution split
        # what is the wealth of each element of lcf at the end of savings phase?
        spcf <- list()
        spcf$wealth <- rev(lcf)%*%apply(1+SPFretsel[nrow(SPFretsel):1,],2,function(x){c(1,(cumprod(x)))}) # replace rev&rev to make muuuuch faster
        #dim(spcf$wealth)<-c(1,dim(ret)[3]);
        rownames(spcf$wealth) <- (ret_age-1)
        # Now calculate cashflows after ret_age
        # lumpsum payment from wealth_at ret_age
        spcf$lumpsum <- spcf$wealth*(1-nu2)
        # annuitization of rest including adjustment for early/late retirement
        #sp_pensionstart <- spcf$wealth*nu2*rho2*(1+0.126*(ret_age-65)) #last term is early/late retirement adjustment of SPL
        # adjustment for inflation: simplifying assumption of "no inflation adjustment"
        # no inflation adjustment leads to deflation in real terms
        #infl <- ret[ret_age:122,"infl",]
        #h2 <- apply(infl,2,function(x) exp(-cumsum(x/2)))
        # h2 <- exp(-apply(ret[ret_age:122,"infl",]/2,2,function(x) cumsum(x)))
        # rownames(h2) <- as.character(ret_age:122)
        spcf$pension <- matrix(spcf$wealth*nu2*rho2*(1+0.126*(ret_age-65)),byrow=TRUE,nrow=length(ret_age:122),ncol=dim(retr)[3])*exp(-apply(ret[ret_age:122,"infl",]/2,2,function(x) cumsum(x)))
        rownames(spcf$pension) <- as.character(ret_age:122)
      ### 3. Third Pillar
      free_cf_before_tax <- setNames(li*(1+lg)^(seq(sav_years)-1)*(1-c1-c2),c_age:(ret_age-1))
      #names(free_cf_before_tax) <- c_age:(ret_age-1)
      ## 3a. CF during working phase
        # includes wealth development and consumption development during working phase
        retr[,"libor",] <- retr[,"libor",] + as.numeric(w3["libor"] < 0)*psi
        # portfolio returns (real) during saving years
        #ma <- exp(retr[c_age:(ret_age-1),names(w3),,drop=FALSE])-1
        pf_ret <- apply(exp(retr[c_age:(ret_age-1),names(w3),,drop=FALSE])-1,3,function(x) x%*%w3) # discrete returns times portfolio weights
        # interest for negative liquid wealth
        #ma_d <- exp(retr[c_age:(ret_age-1),"libor",,drop=FALSE]+as.numeric(w3["libor"] >= 0)*psi)-1
        debt <- apply(exp(retr[c_age:(ret_age-1),"libor",,drop=FALSE]+as.numeric(w3["libor"] >= 0)*psi)-1,3,function(x) x)
        dim(pf_ret) <- dim(debt) <- c(length(c_age:(ret_age-1)),dim(retr)[3])
        # cf<-c(s3,freecfbeforetax)  #what for? (where used again?)
        #########################################
        ## 3. Wealth and Cash Flows
        wealth_development <- consumption <- array(NA,c(ret_age-c_age,dim(retr)[3]))
        rownames(wealth_development) <- rownames(consumption) <- as.character(seq(c_age,ret_age-1))
        # In first year
        cf_tax <- taxCFwork(free_cf_before_tax[1], liquid_wealth = s3, illiquid_wealth = w0) # wealth from 1.1.
        free_cf_after_tax <- free_cf_before_tax[1] - cf_tax$from_cf
        # if s3<0: pay libor + psi. If s3 >0 invest according to w3/pf_ret
        wealth_development[1,] <- as.numeric(s3>0)*s3*(1+pf_ret[1,]) + as.numeric(s3<=0)*s3*(1+debt[1,]) + (1-c)*free_cf_after_tax - cf_tax$from_liquid_wealth
        consumption[1,] <- c*free_cf_after_tax
        # in all folowing years
        if(sav_years>=2){
          for(age in seq(c_age+1,ret_age-1)){
            t <- age - c_age
            # cash flow = income minus taxes (wealth from last period)
            cf_tax <- taxCFwork(free_cf_before_tax[t+1], liquid_wealth = wealth_development[t,], illiquid_wealth = w0)
            free_cf_after_tax <- free_cf_before_tax[t+1] - cf_tax$from_cf
            wealth_development[t+1,] <- as.numeric(wealth_development[t,]>0)*wealth_development[t,]*(1+pf_ret[1,]) +
              as.numeric(wealth_development[t,]<=0)*wealth_development[t,]*(1+debt[1,]) + (1-c)*free_cf_after_tax - cf_tax$from_liquid_wealth
            consumption[t+1,] <- c*free_cf_after_tax
          }
        }
        tpcfw <- list()
        tpcfw$cons <- consumption
        #tpcfw$wealth <- wealth_development
    tcf<-list()
    tcf$wealth_before_ret <- wealth_development #tpcfw$wealth
      ## third pillar lumpsum is wealth that is NOT converted to life-long pension (nu3: how much will be converted to life-long pension)
      # for the case that wealth is negative do not allow for annuitization of anything
      tp_wealth_befor_pension <- tcf$wealth_before_ret[as.character(ret_age-1),,drop=FALSE]
      # if wealth>0 allow for annuitization of nu3, if <0 put it all to the lumpsum
      tp_lumpsum <- tp_wealth_befor_pension * (1 - nu3*as.numeric(tp_wealth_befor_pension>=0))
      ## lumpsum after tax
      # 2nd pillar tax (special tax treatment for lump sum payments from second pillar, assumption: no insurance products in third pillar!)
      # 3rd pillar lumpsum is just wealth at retirement after reduction by nu3
      lumpsum_after_tax <- taxCFlumpsum(lumpsum = spcf$lumpsum, gender = gender,ret_age = ret_age, warnings=warnings) + tp_lumpsum
      ## 3rd pillar annuity is wealth converted to life-long pension using conversion rate rho3 and early/late retirement adjustment of 2nd pillar
      # we create a timeseries from it
      tp_pension_ann <- (spcf$pension*0+1)[,1,drop=FALSE] %*% ((tp_wealth_befor_pension*nu3*as.numeric(tp_wealth_befor_pension>=0)*rho3*(1+0.126*(ret_age-65))))
      ## 3b. CF during retirement
        pf_ret <- apply((exp(retr[(ret_age+1):122,names(w3),,drop=FALSE])-1),3,function(x) x%*%w3) # discrete returns times portfolio weights
        debt <- apply((exp(retr[(ret_age+1):122,"libor",,drop=FALSE]+as.numeric(w3["libor"] >= 0)*psi)-1),3,function(x) x)
        # necessary to keep matrix dimensions
        dim(pf_ret) <- dim(debt) <- c(length((ret_age+1):122),dim(retr)[3])
        #
        cfact3 <- alpha # !!! alpha = const!!!
        ## 1) In case the final wealth is positive we take away cfact3 from wealth before we add interest
        # 2) in case of negative wealth we do not allow for consumption but charge libor
        # Could be adapted as follows: alpha could be applied to pay back credit from the other pensions
        # also: we could still consume (aka reverse mortgage) by borrowing on w0 (lets say up to 80%*w0).
        # In all cases we leave negative wealth as bequest and therefore have to adapt the utility function
        ## Step 1 adapt e
        e1 <- apply((1+pf_ret)*cfact3,2,function(x){cumprod(x)})
        e1 <- rbind(e1[1,]*0+1,e1) # this is ultimatively wrong but looks better. for correct treatment delete here and delete "+1" in lines 47f
        e2 <- apply((1+debt),2,function(x){cumprod(x)})
        e2 <- rbind(e2[1,]*0+1,e2) # this is ultimatively wrong but looks better. for correct treatment delete here and delete "+1" in lines 47f
        e <- matrix(as.numeric(lumpsum_after_tax>=0),nrow=nrow(e1),ncol=ncol(e1),byrow=TRUE)*e1 +
          matrix(as.numeric(lumpsum_after_tax<0),nrow=nrow(e2),ncol=ncol(e2),byrow=TRUE)*e2
        wealth <- matrix(lumpsum_after_tax,byrow = TRUE,nrow=nrow(e),ncol=ncol(e))*e
        rownames(wealth) <- as.character(ret_age:122)
        consumption <- matrix(as.numeric(lumpsum_after_tax>=0),nrow=nrow(e1),ncol=ncol(e1),byrow=TRUE)*apply(wealth,2,function(x) x*(1-cfact3))
        # create output
        tpcf_ret <- list()
        tpcf_ret$cons <- consumption
        tpcf_ret$wealth <- wealth


      cf_ret <- fpcf + spcf$pension + tp_pension_ann
      ret_tax <- taxCFret(fpcf = fpcf, totalcf = cf_ret, wealth = tpcf_ret$wealth + w0, warnings=warnings)
      cf_ret_after_tax <- cf_ret - ret_tax$from_cf
    tcf$wealth_after_ret <- tpcf_ret$wealth - ret_tax$from_wealth
    ### 4. Consumption
    tcf$cons <- rbind(tpcfw$cons, tpcf_ret$cons + cf_ret_after_tax)

    ################ UTIL
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
      sur <- cumprod(1-gender_mortalityTable2[(c_age-1):(length(gender_mortalityTable2[,gender+1])-1),gender+1])
      #########################################
      ## 3. Calculate utilities
      # find those that have negative cons or wealth
      wealth <- rbind(tcf$wealth_before_ret,tcf$wealth_after_ret)
      uncondmort <- c(1,sur)*gender_mortalityTable2[(c_age-1):length(gender_mortalityTable2[,gender+1]),gender+1]
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
#' Helper 0: Sel SPFret
#'
#' @export
.SPFretch <- function(SPFret,c_age,ret_age){
  return(SPFret[[as.character(c_age)]][[as.character(ret_age)]])
}
#' Helper 1: Optimization function
#'
#' @param inputvec c(c, c2, nu2, nu3, alpha, w3)
#'
.util_optim2 <- function(inputvec,ret_age,ra,delta,beta,c_age,gender,gender_mortalityTable2,w0,CF,li,lg,c1,s1,s2,s3,rho2,rho3,ret,retr,SPFretsel,psi,verbose=FALSE, warnings=FALSE){
  #ret_age,tw3,c,c2,nu2,nu3,delta,alpha
  # inputvec=c(ret_age,c,c2,nu2,nu3,alpha,tw3)
  return(-utilall(ret_age=ret_age,tw3=inputvec[6:8],c=inputvec[1],c2=inputvec[2],nu2=inputvec[3],nu3=inputvec[4],ra=ra,delta=delta,alpha=inputvec[5],
              beta=beta,c_age=c_age,gender=gender,
              gender_mortalityTable2=gender_mortalityTable2,w0=w0,CF=CF,li=li,lg=lg,c1=c1,s1=s1,s2=s2,s3=s3,rho2=rho2,rho3=rho3,
              ret=ret,retr=retr,SPFretsel=SPFretsel,psi=psi,verbose=verbose, warnings=warnings))
}
#' Helper 2: Optimize portfolio weights and consumption c,
#'
#' @param inputvec c(c, alpha, w3)
#'
.util_optim_wc2 <- function(inputvec,ret_age,c2,nu2,nu3,ra,delta,beta,c_age,gender,gender_mortalityTable2,w0,CF,li,lg,c1,s1,s2,s3,rho2,rho3,ret,retr,SPFretsel,psi,verbose=FALSE, warnings=FALSE){
  #ret_age,tw3,c,c2,nu2,nu3,delta,alpha
  # inputvec=c(ret_age,c,c2,nu2,nu3,alpha,tw3)
  return(-utilall(ret_age=ret_age,tw3=inputvec[3:5],c=inputvec[1],c2=c2,nu2=nu2,nu3=nu3,ra=ra,delta=delta,alpha=inputvec[2],
               beta=beta,c_age=c_age,gender=gender,
               gender_mortalityTable2=gender_mortalityTable2,w0=w0,CF=CF,li=li,lg=lg,c1=c1,s1=s1,s2=s2,s3=s3,rho2=rho2,rho3=rho3,
               ret=ret,retr=retr,SPFretsel=SPFretsel,psi=psi,verbose=verbose, warnings=warnings))
}
#' Helper 3: Optimize only portfolio weights
#'
#' @param inputvec c(w3)
#'
.util_optim_w2 <- function(inputvec,ret_age,cc,c2,nu2,nu3,ra,delta,alpha,beta,c_age,gender,gender_mortalityTable2,w0,CF,li,lg,c1,s1,s2,s3,rho2,rho3,ret,retr,SPFretsel,psi,verbose=FALSE, warnings=FALSE){
  #ret_age,tw3,c,c2,nu2,nu3,delta,alpha
  # inputvec=c(ret_age,c,c2,nu2,nu3,alpha,tw3)
  return(-utilall(ret_age=ret_age,tw3=inputvec,c=cc,c2=c2,nu2=nu2,nu3=nu3,ra=ra,delta=delta,alpha=alpha,
              beta=beta,c_age=c_age,gender=gender,
              gender_mortalityTable2=gender_mortalityTable2,w0=w0,CF=CF,li=li,lg=lg,c1=c1,s1=s1,s2=s2,s3=s3,rho2=rho2,rho3=rho3,
              ret=ret,retr=retr,SPFretsel=SPFretsel,psi=psi,verbose=verbose, warnings=warnings))
}
#################### CHECKING the utility function
# MortalityTables::mortalityTables.load("Austria_Annuities")
# gender_mortalityTable <- MortalityTables::baseTable(AVOe2005R.male)
# # 1) Higher retirement age - does lead to decreasing utility - due to not consuming the lage amounts of my wealth
# out4 <- NULL; vec <- 60:70
# for (ret_age in vec){
#   out <- util(ret_age=ret_age,c_age=42,tw3=c(0.25,0.25,0.25),c=0.6,c2=0.12,nu2=.5,nu3=0.01,ra=4,delta=0.02,alpha=0.96,
#               beta=0.75,gender=1,gender_mortalityTable=gender_mortalityTable,w0=300000,CF=0,li=100000,lg=0.01,c1=0.07,s1=c(15,80000),s2=30000,s3=30000,
#               w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
#               rho2=0.05,rho3=0.04,ret=ret[,,1:10],retr=retr[,,1:10],psi=0.015,verbose=FALSE)
#   out4 <- rbind(out4,out)
# }
# par(mfrow=c(1,1),oma=c(0,0,0,0),mar=c(2,2,2,1))
# plot(vec,out4[,1],main = "Utility",type="l")
# # 2) Higher current age leads to an inverse u-shape (given the other parameters, the best age seems to be ~55)
# out4 <- NULL; vec <- 42:64
# for (c_age in vec){
#   out <- util(ret_age=65,c_age=c_age,tw3=c(0.25,0.25,0.25),c=0.6,c2=0.12,nu2=.5,nu3=0.01,ra=4,delta=0.02,alpha=0.96,
#               beta=0.75,gender=1,gender_mortalityTable=gender_mortalityTable,w0=300000,CF=0,li=100000,lg=0.01,c1=0.07,s1=c(15,80000),s2=30000,s3=30000,
#               w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
#               rho2=0.05,rho3=0.04,ret=ret[,,1:100],retr=retr[,,1:100],psi=0.015,verbose=FALSE)
#   out4 <- rbind(out4,out)
# }
# plot(vec,out4[,1],main = "Utility",type="l")
#   # Request for "consumption of everything: One needs low enough wealth to be achieving an optimum here. What about habit formation?
# # 3) Higher consumption during work life leads to too little savings for retirement which therefore peaks
# out4 <- NULL; vec <- seq(0.6,1,0.01)
# for (c in vec){
#   out <- util(ret_age=65,c_age=42,tw3=c(0.25,0.25,0.25),c=c,c2=0.12,nu2=.5,nu3=0.01,ra=4,delta=0.02,alpha=0.96,
#               beta=0.75,gender=1,gender_mortalityTable=gender_mortalityTable,w0=300000,CF=0,li=100000,lg=0.01,c1=0.07,s1=c(15,80000),s2=30000,s3=30000,
#               w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
#               rho2=0.05,rho3=0.04,ret=ret[,,1:10],retr=retr[,,1:10],psi=0.015,verbose=FALSE)
#   out4 <- rbind(out4,out)
# }
# plot(vec,out4[,1],main = "Utility",type="l")
# # 4) How much should be contributed to the second pillar? When wealth is low enough it peaks between 0 and 5 percent
# out4 <- NULL; vec <- seq(0,0.15,0.01)
# for (c2 in vec){
#   out <- util(ret_age=65,c_age=42,tw3=c(0.25,0.25,0.25),c=0.6,c2=c2,nu2=.5,nu3=0.01,ra=4,delta=0.02,alpha=0.96,
#               beta=0.75,gender=1,gender_mortalityTable=gender_mortalityTable,w0=300000,CF=0,li=100000,lg=0.01,c1=0.07,s1=c(15,80000),s2=30000,s3=30000,
#               w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
#               rho2=0.05,rho3=0.04,ret=ret[,,1:10],retr=retr[,,1:10],psi=0.015,verbose=FALSE)
#   out4 <- rbind(out4,out)
# }
# plot(vec,out4[,1],main = "Utility",type="l")
# # 5) How much of second pillar savings shall be payed out as a lumpsum?
# out4 <- NULL; vec <- seq(0,1,0.05)
# for (nu2 in vec){
#   out <- util(ret_age=65,c_age=42,tw3=c(0.25,0.25,0.25),c=0.6,c2=0.12,nu2=nu2,nu3=0.01,ra=4,delta=0.02,alpha=0.96,
#               beta=0.75,gender=1,gender_mortalityTable=gender_mortalityTable,w0=30000,CF=0,li=100000,lg=0.01,c1=0.07,s1=c(15,80000),s2=30000,s3=-100000,
#               w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
#               rho2=0.05,rho3=0.04,ret=ret[,,1:10],retr=retr[,,1:10],psi=0.015,verbose=FALSE)
#   out4 <- rbind(out4,out)
# }
# plot(vec,out4[,1],main = "Utility",type="l")
# # 6) How much of third pillar savings shall be payed out as a lumpsum?
# out4 <- NULL; vec <- seq(0,1,0.05)
# for (nu3 in vec){
#   out <- util(ret_age=65,c_age=42,tw3=c(0.25,0.25,0.25),c=0.6,c2=0.12,nu2=.5,nu3=nu3,ra=4,delta=0.02,alpha=0.96,
#               beta=0.75,gender=1,gender_mortalityTable=gender_mortalityTable,w0=30000,CF=0,li=100000,lg=0.01,c1=0.07,s1=c(15,80000),s2=30000,s3=30000,
#               w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
#               rho2=0.05,rho3=0.04,ret=ret[,,1:10],retr=retr[,,1:10],psi=0.015,verbose=FALSE)
#   out4 <- rbind(out4,out)
# }
# plot(vec,out4[,1],main = "Utility",type="l")
# # 7) Risk aversion. Larger risk aversion leads to ??? [higher utility]
# out4 <- NULL; vec <- seq(2,20,1)
# for (ra in vec){
#   out <- util(ret_age=65,c_age=42,tw3=c(0.25,0.25,0.25),c=0.6,c2=0.12,nu2=.5,nu3=0.1,ra=ra,delta=0.02,alpha=0.96,
#               beta=0.75,gender=1,gender_mortalityTable=gender_mortalityTable,w0=30000,CF=0,li=100000,lg=0.01,c1=0.07,s1=c(15,80000),s2=30000,s3=10000,
#               w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
#               rho2=0.05,rho3=0.04,ret=ret[,,1:10],retr=retr[,,1:10],psi=0.015,verbose=FALSE)
#   out4 <- rbind(out4,out)
# }
# plot(vec,out4[,1],main = "Utility",type="l")
# # 8) Time preference. Larger time preference decreases utility ?
# out4 <- NULL; vec <- seq(0,0.1,0.01)
# for (delta in vec){
#   out <- util(ret_age=65,c_age=42,tw3=c(0.25,0.25,0.25),c=0.6,c2=0.12,nu2=.5,nu3=0.1,ra=4,delta=delta,alpha=0.96,
#               beta=0.75,gender=1,gender_mortalityTable=gender_mortalityTable,w0=30000,CF=0,li=100000,lg=0.01,c1=0.07,s1=c(15,80000),s2=30000,s3=30000,
#               w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
#               rho2=0.05,rho3=0.04,ret=ret[,,1:10],retr=retr[,,1:10],psi=0.015,verbose=FALSE)
#   out4 <- rbind(out4,out)
# }
# plot(vec,out4[,1],main = "Utility",type="l")
# # 9) Alpha. Larger alpha (means you consume less "self-managed pension") utility peaks between 0.9 and 0.99
# out4 <- NULL; vec <- seq(0.80,0.99,0.01)
# for (alpha in vec){
#   out <- util(ret_age=65,c_age=42,tw3=c(0.25,0.25,0.25),c=0.6,c2=0.12,nu2=.5,nu3=0.1,ra=4,delta=0.02,alpha=alpha,
#               beta=0.75,gender=1,gender_mortalityTable=gender_mortalityTable,w0=30000,CF=0,li=100000,lg=0.01,c1=0.07,s1=c(15,80000),s2=30000,s3=-10000,
#               w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
#               rho2=0.05,rho3=0.04,ret=ret[,,1:10],retr=retr[,,1:10],psi=0.015,verbose=FALSE)
#   out4 <- rbind(out4,out)
# }
# plot(vec,out4[,1],main = "Utility",type="l")
# # 10) Beta. Larger beta (means you consume less "self-managed pension") decreases utility (you leave more to your heirs and consume less yourself)
# out4 <- NULL; vec <- seq(0,1,0.1)
# for (beta in vec){
#   out <- util(ret_age=65,c_age=42,tw3=c(0.25,0.25,0.25),c=0.6,c2=0.12,nu2=.5,nu3=0.1,ra=4,delta=0.02,alpha=0.96,
#               beta=beta,gender=1,gender_mortalityTable=gender_mortalityTable,w0=30000,CF=0,li=100000,lg=0.01,c1=0.07,s1=c(15,80000),s2=30000,s3=30000,
#               w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
#               rho2=0.05,rho3=0.04,ret=ret[,,1:10],retr=retr[,,1:10],psi=0.015,verbose=FALSE)
#   out4 <- rbind(out4,out)
# }
# plot(vec,out4[,1],main = "Utility",type="l")
# # 11) Wealth. Larger non-liquid wealth (means you consume less "self-managed pension") decreases utility (due to increasing taxes) but does not provide
# # "illiquid pension" (as a mortgage is usually treated)
# out4 <- NULL; vec <- seq(0,100000,10000)
# for (w0 in vec){
#   out <- util(ret_age=65,c_age=42,tw3=c(0.25,0.25,0.25),c=0.6,c2=0.12,nu2=.5,nu3=0.1,ra=4,delta=0.02,alpha=0.96,
#               beta=0.75,gender=1,gender_mortalityTable=gender_mortalityTable,w0=w0,CF=0,li=100000,lg=0.01,c1=0.07,s1=c(15,80000),s2=30000,s3=-10000,
#               w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
#               rho2=0.05,rho3=0.04,ret=ret[,,1:10],retr=retr[,,1:10],psi=0.015,verbose=FALSE)
#   out4 <- rbind(out4,out)
# }
# plot(vec,out4[,1],main = "Utility",type="l")
# # 12) Income. Higher income higher utility (starting above the benefits from fp)
# out4 <- NULL; vec <- seq(30000,100000,10000)
# for (li in vec){
#   out <- util(ret_age=65,c_age=42,tw3=c(0.25,0.25,0.25),c=0.6,c2=0.12,nu2=.5,nu3=0.1,ra=4,delta=0.02,alpha=0.96,
#               beta=0.75,gender=1,gender_mortalityTable=gender_mortalityTable,w0=30000,CF=0,li=li,lg=0.01,c1=0.07,s1=c(15,80000),s2=30000,s3=-10000,
#               w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
#               rho2=0.05,rho3=0.04,ret=ret[,,1:10],retr=retr[,,1:10],psi=0.015,verbose=FALSE)
#   out4 <- rbind(out4,out)
# }
# plot(vec,out4[,1],main = "Utility",type="l")
# # 13) Income growth. Higher income growth higher utility
# out4 <- NULL; vec <- seq(0,0.1,0.01)
# for (lg in vec){
#   out <- util(ret_age=65,c_age=42,tw3=c(0.25,0.25,0.25),c=0.6,c2=0.12,nu2=.5,nu3=0.1,ra=4,delta=0.02,alpha=0.96,
#               beta=0.75,gender=1,gender_mortalityTable=gender_mortalityTable,w0=30000,CF=0,li=100000,lg=lg,c1=0.07,s1=c(15,80000),s2=30000,s3=-10000,
#               w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
#               rho2=0.05,rho3=0.04,ret=ret[,,1:10],retr=retr[,,1:10],psi=0.015,verbose=FALSE)
#   out4 <- rbind(out4,out)
# }
# plot(vec,out4[,1],main = "Utility",type="l")
# # 14) More first pillar saving, lower utility
# out4 <- NULL; vec <- seq(0,0.1,0.01)
# for (c1 in vec){
#   out <- util(ret_age=65,c_age=42,tw3=c(0.25,0.25,0.25),c=0.6,c2=0.12,nu2=.5,nu3=0.1,ra=4,delta=0.02,alpha=0.96,
#               beta=0.75,gender=1,gender_mortalityTable=gender_mortalityTable,w0=30000,CF=0,li=100000,lg=0.01,c1=c1,s1=c(15,80000),s2=30000,s3=-10000,
#               w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
#               rho2=0.05,rho3=0.04,ret=ret[,,1:10],retr=retr[,,1:10],psi=0.015,verbose=FALSE)
#   out4 <- rbind(out4,out)
# }
# plot(vec,out4[,1],main = "Utility",type="l")
# # 15) More first pillar savings, rising utility (in very tight boundaries on li etc!)
# out4 <- NULL; vec <- seq(100,10000,1000)
# for (s12 in vec){
#   out <- util(ret_age=65,c_age=42,tw3=c(0.25,0.25,0.25),c=0.6,c2=0.12,nu2=.5,nu3=0.1,ra=4,delta=0.02,alpha=0.96,
#               beta=0.75,gender=1,gender_mortalityTable=gender_mortalityTable,w0=30000,CF=0,li=10000,lg=0.01,c1=0.07,s1=c(10,s12),s2=30000,s3=-10000,
#               w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
#               rho2=0.05,rho3=0.04,ret=ret[,,1:10],retr=retr[,,1:10],psi=0.015,verbose=FALSE)
#   out4 <- rbind(out4,out)
# }
# plot(vec,out4[,1],main = "Utility",type="l")
# # 16) More second pillar savings, higher utility (decreasing)
# out4 <- NULL; vec <- seq(0,100000,10000)
# for (s2 in vec){
#   out <- util(ret_age=65,c_age=42,tw3=c(0.25,0.25,0.25),c=0.6,c2=0.12,nu2=.5,nu3=0.1,ra=4,delta=0.02,alpha=0.96,
#               beta=0.75,gender=1,gender_mortalityTable=gender_mortalityTable,w0=30000,CF=0,li=100000,lg=0.01,c1=0.07,s1=c(15,80000),s2=s2,s3=-10000,
#               w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
#               rho2=0.05,rho3=0.04,ret=ret[,,1:10],retr=retr[,,1:10],psi=0.015,verbose=FALSE)
#   out4 <- rbind(out4,out)
# }
# plot(vec,out4[,1],main = "Utility",type="l")
# # 17) More third pillar savings, higher utility? There seems to be a "worst amount" of s3?
# out4 <- NULL; vec <- seq(-100000,100000,10000)
# for (s3 in vec){
#   out <- util(ret_age=65,c_age=42,tw3=c(0.25,0.25,0.25),c=0.6,c2=0.12,nu2=.5,nu3=0.1,ra=4,delta=0.02,alpha=0.96,
#               beta=0.75,gender=1,gender_mortalityTable=gender_mortalityTable,w0=30000,CF=0,li=100000,lg=0.01,c1=0.01,s1=c(15,80000),s2=30000,s3=s3,
#               w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
#               rho2=0.05,rho3=0.04,ret=ret[,,1:10],retr=retr[,,1:10],psi=0.015,verbose=FALSE)
#   out4 <- rbind(out4,out)
# }
# plot(vec,out4[,1],main = "Utility",type="l")
# # 18) Higher conversion rate, higher utility (takes the risk away)
# out4 <- NULL; vec <- seq(0,0.1,0.01)
# for (rho2 in vec){
#   out <- util(ret_age=65,c_age=42,tw3=c(0.25,0.25,0.25),c=0.6,c2=0.12,nu2=.5,nu3=0.1,ra=4,delta=0.02,alpha=0.96,
#               beta=0.75,gender=1,gender_mortalityTable=gender_mortalityTable,w0=30000,CF=0,li=100000,lg=0.01,c1=0.07,s1=c(15,80000),s2=30000,s3=-10000,
#               w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
#               rho2=rho2,rho3=0.04,ret=ret[,,1:10],retr=retr[,,1:10],psi=0.015,verbose=FALSE)
#   out4 <- rbind(out4,out)
# }
# plot(vec,out4[,1],main = "Utility",type="l")
# # 19) Higher conversion rate, higher utility (takes the risk away)
# out4 <- NULL; vec <- seq(0,0.1,0.01)
# for (rho3 in vec){
#   out <- util(ret_age=65,c_age=42,tw3=c(0.25,0.25,0.25),c=0.6,c2=0.12,nu2=.5,nu3=0.1,ra=4,delta=0.02,alpha=0.96,
#               beta=0.75,gender=1,gender_mortalityTable=gender_mortalityTable,w0=30000,CF=0,li=100000,lg=0.01,c1=0.07,s1=c(15,80000),s2=30000,s3=-10000,
#               w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
#               rho2=0.05,rho3=rho3,ret=ret[,,1:10],retr=retr[,,1:10],psi=0.015,verbose=FALSE)
#   out4 <- rbind(out4,out)
# }
# plot(vec,out4[,1],main = "Utility",type="l")
# # 20) Larger investment
# out4 <- NULL; vec <- seq(-1,1,0.1)
# for (xw3 in vec){
#   out <- util(ret_age=65,c_age=42,tw3=c(xw3,xw3,xw3),c=0.6,c2=0.12,nu2=.5,nu3=0.1,ra=4,delta=0.02,alpha=0.96,
#               beta=0.75,gender=1,gender_mortalityTable=gender_mortalityTable,w0=30000,CF=0,li=100000,lg=0.01,c1=0.07,s1=c(15,80000),s2=30000,s3=-10000,
#               w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
#               rho2=0.05,rho3=0.04,ret=ret[,,1:10],retr=retr[,,1:10],psi=0.015,verbose=FALSE)
#   out4 <- rbind(out4,out)
# }
# plot(vec,out4[,1],main = "Utility",type="l")
