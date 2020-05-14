#' Total Cash Flow Calculation
#'
#' This function calculates the total cash flows (for each scenario and working age/retirment) given all
#' input parameters nedding all previously defined functions
#'
#' @param ret_age OptVar: retirement age, can be set anywhere between 60 and 70 (default: 65)
#' @param w3 OptVar: third pillar portfolio allocation (given either as vector or as matrix with entries) for all years
#' @param c OptVar: fraction of income that is consumed while still working (current assumption: constant)
#' @param c2 OptVar: second pillar savings as fraction of gross income (still missing: health, a-fonds-perdu payments)
#' @param nu2 OptVar: fraction of second pillar savings that is converted to life-long pension
#' @param nu3 OptVar: fraction of third pillar savings that is converted to life-long pension
#' @param c_age GivenVar: the investor's current age (assuming birthday is calculation-day)
#' @param gender GivenVar: gender, 0=male and 1=female
#' @param w0 GivenVar: time c_age wealth that is not disposable, assumption: still available at retirement (no growth or decline),
#' alternatively: expected wealth (that is not disposable) at retirement, stays the same over time
#' @param CF Given Variables: income shocks, such as inheritance (not currrently imlemented)
#' @param li GivenVar: gross labor income at time 0 (in the last year before birthday)
#' @param lg GivenVar: labor growth rate (in real terms, constant)
#' @param c1 GivenVar: first pillar savings as fraction of gross income
#' @param s1 GivenVar: vector consisting of two components: c(number of contribution years at age=c_age,historical average yearly income until c_age)
#' @param s2 GivenVar: savings in second pillar as of t=0
#' @param s3 GivenVar: liquid wealth - invested in the third pillar (current assumption: no tax advantage for third pillar)
#' @param w2 GivenVar: portfolio allocation in second pillar (assumed to be fixed and not influenced by the decision maker)
#' @param rho2 GivenVar: conversion factor in second pillar for regular retirement age
#' @param rho3 GivenVar: conversion factor in third pillar for regular retirement age
#' @param ret GivenVar: investment return scenarios (nominal)
#' @param retr GivenVar: investment return scenarios (real)
#' @param psi optional, spread to take a loan/leverage for third pillar savings
#' @param alpha parameter to choose fraction of wealth NOT consumed during retirement but kept for investment (and subsequent consumption) - see file 'Consumption_3p.ods'
#' @param warnings optional: should warnings be given? (default=TRUE)
#'
#' @return returns a list with three elements:
#' - a matrix of consumptions before and after retirement (dim c_age:122, # of scenarios)
#' - a matrix of wealth after retirement (dim ret_age:122, # of scenarios)
#' - a matrix of wealth before retirement (dim c_age:(ret_age-1), # of scenarios)
#'
#' @examples
#' data(ret);data(retr)
#' totalcf_test <- totalCF(ret_age=65,c_age=42,
#'                 w3=setNames(c(.25,.25,.25,.25,0),c("msci","b10","recom","libor","infl")),
#'                 c=0.6,c2=.12,nu2=.5,nu3=0.01,gender=0,
#'                 w0=300000,li=100000,lg=0.01,c1=0.07,s1=c(15,80000),s2=300000,s3=300000,
#'                 w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
#'                 rho2=0.05,rho3=0.04,ret=ret[,,1:10],retr=retr[,,1:10],psi=0.015,alpha=0.96)
#' totalcf_test2 <- totalCF(ret_age=65,c_age=64,
#'                 w3=setNames(c(1,0,0,0,0),c("msci","b10","recom","libor","infl")),
#'                 c=1,c2=0,nu2=0,nu3=0,gender=0,
#'                 w0=0,li=100,lg=0,c1=0,s1=c(0,0),s2=0,s3=0,
#'                 w2=setNames(c(0,0,0,1,0),c("msci","b10","recom","libor","infl")),
#'                 rho2=0.01,rho3=0.01,ret=ret[,,1:10],retr=retr[,,1:10],psi=0.015,alpha=0.96)
#'
#' @export
totalCF <- function(ret_age,w3,c,c2,nu2,nu3,c_age,gender,w0,CF=NULL,li,lg,c1,s1,s2,s3,w2,rho2,rho3,ret,retr,psi,alpha, warnings=TRUE){
  # labor = gross labor income, cofrag = consumption fraction of net income
  # cont 2 = employee contribution second pillar of gross income [0,1] ca. 0.08-0.12 (modeled as scalar)
  # net income = income - first and second pillar contribution - tax
  # consumption (net income*cofrag) - rest to tpCF
  #########################################
  ## 0. checks
  if (warnings){
    if ((nu3 < 0)|(nu3 > 1)) stop ("'nu2' lumpsum vs annuity decision must be between 0 and 1")
    if ((rho2 <= 0)|(rho3 > 0.2)) warning ("'rho2' conversion rate should be larger than zero and smaller than 0.2 (which is already an extreme case)")
    if ((rho3 <= 0)|(rho3 > 0.2)) warning ("'rho3' conversion rate should be larger than zero and smaller than 0.2 (which is already an extreme case)")
    if (!is.null(CF)) warning('Onetime cashflows are not yet modelled and will not be considered')
  }
  #########################################
  ## 1. Pre-Calculations
  # years left for saving
  sav_years <- ret_age - c_age
  #########################################
  ## 2. Cash-Flows
  ### 2a. First Pillar
    fpcf <- fpCF(ret_age = ret_age, c_age = c_age, li=li, lg=lg, s1=s1, ret=ret, warnings=warnings)
  ### 2b. Second Pillar
    spcf <- spCF(ret_age = ret_age,nu2 = nu2,c_age = c_age,c2=c2,li=li,lg=lg,w2=w2,ret=ret,retr=retr,s2=s2,rho2=rho2, warnings=warnings)
  ### 3. Third Pillar
    free_cf_before_tax <- li*(1+lg)^(seq(sav_years)-1)*(1-c1-c2)
      names(free_cf_before_tax) <- c_age:(ret_age-1)
    ## 3a. CF during working phase
      # includes wealth development and consumption development during working phase
      tpcfw <- tpCFwork(ret_age = ret_age, c_age = c_age, w3 = w3,
                        free_cf_before_tax = free_cf_before_tax,
                        retr=retr, s3=s3, w0=w0, psi=psi, c=c, warnings=warnings)
      ## third pillar lumpsum is wealth that is NOT converted to life-long pension (nu3: how much will be converted to life-long pension)
      # for the case that wealth is negative do not allow for annuitization of anything
      tp_wealth_for_pension <- tpcfw$wealth[as.character(ret_age-1),,drop=FALSE]
      # if wealth>0 allow for annuitization of nu3, if <0 put it all to the lumpsum
      tp_lumpsum <- tp_wealth_for_pension * (1 - nu3*as.numeric(tp_wealth_for_pension>=0))
      ## lumpsum after tax
      # 2nd pillar tax (special tax treatment for lump sum payments from second pillar, assumption: no insurance products in third pillar!)
      # 3rd pillar lumpsum is just wealth at retirement after reduction by nu3
      lumpsum_after_tax <- taxCFlumpsum(lumpsum = spcf$lumpsum, gender = gender,ret_age = ret_age, warnings=warnings) + tp_lumpsum
      ## 3rd pillar annuity is wealth converted to life-long pension using conversion rate rho3 and early/late retirement adjustment of 2nd pillar
      # we create a timeseries from it
      tp_pension_ann <- (spcf$pension*0+1)[,1,drop=FALSE] %*% ((tp_wealth_for_pension*nu3*as.numeric(tp_wealth_for_pension>=0)*rho3*(1+0.126*(ret_age-65))))
    ## 3b. CF during retirement
    tpcf_ret <- tpCFret(ret_age = ret_age, c_age = c_age, w3 = w3, alpha = alpha,
                        wealth_at_ret_age = lumpsum_after_tax,
                        retr = retr, psi=psi, warnings=warnings)
    cf_ret <- fpcf + spcf$pension + tp_pension_ann
    ret_tax <- taxCFret(fpcf = fpcf, totalcf = cf_ret, wealth = tpcf_ret$wealth + w0, warnings=warnings)
    cf_ret_after_tax <- cf_ret - ret_tax$from_cf
    wealth_after_tax <- tpcf_ret$wealth - ret_tax$from_wealth
  ### 4. Consumption
  consumption <- rbind(tpcfw$cons, tpcf_ret$cons + cf_ret_after_tax)
  # output
  cf<-list()
  cf$cons <- consumption
  cf$wealth_after_ret <- wealth_after_tax
  cf$wealth_before_ret <- tpcfw$wealth
  return(cf)
}
# # 1) Saving longer (higher retirement age) should increase wealth bfore and after retirement (here it depends strongly on investment results) and
# #    decrease consumption at retirement (due to higher wealth taxes)
# out4 <- NULL; vec <- 60:70
# for (ret_age in vec){
#   out <- totalCF(ret_age=ret_age,c_age=42,
#                  w3=setNames(c(.25,.25,.25,.25,0),c("msci","b10","recom","libor","infl")),
#                  c=0.6,c2=.12,nu2=.5,nu3=0.01,gender=0,
#                  w0=300000,li=100000,lg=0.01,c1=0.07,s1=c(15,80000),s2=300000,s3=300000,
#                  w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
#                  rho2=0.05,rho3=0.04,ret=ret[,,1:10],retr=retr[,,1:10],psi=0.015,alpha=0.96)
#   out4 <- rbind(out4,c(mean(out$cons[as.character(ret_age-1),]),mean(out$wealth_before_ret[as.character(ret_age-1),]),mean(out$wealth_after_ret[as.character(ret_age),])))
# }
# par(mfrow=c(3,1))
# plot(vec,out4[,1],main = "Consumption at retirement")
# plot(vec,out4[,2],main = "Wealth before retirement")
# plot(vec,out4[,3],main = "Wealth at retirement")
# # 2) Saving for less time (higher current age) should decrease wealth before and after retirement (here it depends strongly on investment results) and
# #    also decrease consumption at retirement (less wealth, fewer taxes)
# out4 <- NULL; vec <- 42:64
# for (c_age in vec){
#   out <- totalCF(ret_age=65,c_age=c_age,
#                  w3=setNames(c(.25,.25,.25,.25,0),c("msci","b10","recom","libor","infl")),
#                  c=0.6,c2=.12,nu2=.5,nu3=0.01,gender=0,
#                  w0=300000,li=100000,lg=0.01,c1=0.07,s1=c(15,80000),s2=300000,s3=300000,
#                  w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
#                  rho2=0.05,rho3=0.04,ret=ret[,,1:10],retr=retr[,,1:10],psi=0.015,alpha=0.96)
#   out4 <- rbind(out4,c(mean(out$cons["64",]),mean(out$wealth_before_ret["64",]),mean(out$wealth_after_ret["65",])))
# }
# par(mfrow=c(3,1))
# plot(vec,out4[,1],main = "Consumption at retirement")
# plot(vec,out4[,2],main = "Wealth before retirement")
# plot(vec,out4[,3],main = "Wealth at retirement")
# # 3) Consuming more before retirement should decrease wealth before and after retirement (here it depends strongly on investment results) and
# #    also decrease consumption after retirement (less wealth, less taxes, fewer pension payments)
# out4 <- NULL; vec <- seq(0.1,1,0.1)
# for (c in vec){
#   out <- totalCF(ret_age=65,c_age=42,
#                  w3=setNames(c(.25,.25,.25,.25,0),c("msci","b10","recom","libor","infl")),
#                  c=c,c2=.12,nu2=.5,nu3=0.01,gender=0,
#                  w0=300000,li=100000,lg=0.01,c1=0.07,s1=c(15,80000),s2=300000,s3=300000,
#                  w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
#                  rho2=0.05,rho3=0.04,ret=ret[,,1:10],retr=retr[,,1:10],psi=0.015,alpha=0.96)
#   out4 <- rbind(out4,c(mean(out$cons["64",]),mean(out$cons["65",]),
#                        mean(out$wealth_before_ret["64",]),mean(out$wealth_after_ret["65",])))
# }
# par(mfrow=c(4,1),oma=c(0,0,0,0),mar=c(1,2,2,1))
# plot(vec,out4[,1],main = "Consumption before retirement")
# plot(vec,out4[,2],main = "Consumption after retirement")
# plot(vec,out4[,3],main = "Wealth before retirement")
# plot(vec,out4[,4],main = "Wealth at retirement")
# # 3) Saving more in the second pillar should decrease consumption and wealth before retirement, increase wealth after retirement (lumpsum)  and
# #    also increase consumption after retirement (higher annuity)
# out4 <- NULL; vec <- seq(0.1,1,0.1)
# for (c2 in vec){
#   out <- totalCF(ret_age=65,c_age=42,
#                  w3=setNames(c(.25,.25,.25,.25,0),c("msci","b10","recom","libor","infl")),
#                  c=0.6,c2=c2,nu2=.5,nu3=0.01,gender=0,
#                  w0=300000,li=100000,lg=0.01,c1=0.07,s1=c(15,80000),s2=300000,s3=300000,
#                  w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
#                  rho2=0.05,rho3=0.04,ret=ret[,,1:10],retr=retr[,,1:10],psi=0.015,alpha=0.96)
#   out4 <- rbind(out4,c(mean(out$cons["64",]),mean(out$cons["65",]),
#                        mean(out$wealth_before_ret["64",]),mean(out$wealth_after_ret["65",])))
# }
# par(mfrow=c(4,1),oma=c(0,0,0,0),mar=c(1,2,2,1))
# plot(vec,out4[,1],main = "Consumption before retirement")
# plot(vec,out4[,2],main = "Consumption after retirement")
# plot(vec,out4[,3],main = "Wealth before retirement")
# plot(vec,out4[,4],main = "Wealth at retirement")
# # 4) Higher pension (lower lumpsum) in the second pillar should not do anything to consumption and wealth before retirement, decrease wealth after retirement (lumpsum)  and
# #    increase consumption after retirement (higher annuity)
# out4 <- NULL; vec <- seq(0.1,1,0.1)
# for (nu2 in vec){
#   out <- totalCF(ret_age=65,c_age=42,
#                  w3=setNames(c(.25,.25,.25,.25,0),c("msci","b10","recom","libor","infl")),
#                  c=0.6,c2=0.12,nu2=nu2,nu3=0.01,gender=0,
#                  w0=300000,li=100000,lg=0.01,c1=0.07,s1=c(15,80000),s2=300000,s3=300000,
#                  w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
#                  rho2=0.05,rho3=0.04,ret=ret[,,1:10],retr=retr[,,1:10],psi=0.015,alpha=0.96)
#   out4 <- rbind(out4,c(mean(out$cons["64",]),mean(out$cons["65",]),
#                        mean(out$wealth_before_ret["64",]),mean(out$wealth_after_ret["65",])))
# }
# par(mfrow=c(4,1),oma=c(0,0,0,0),mar=c(1,2,2,1))
# plot(vec,out4[,1],main = "Consumption before retirement")
# plot(vec,out4[,2],main = "Consumption after retirement")
# plot(vec,out4[,3],main = "Wealth before retirement")
# plot(vec,out4[,4],main = "Wealth at retirement")
# # 5) Higher pension (lower lumpsum) in the third pillar should not do anything to consumption and wealth before retirement, decrease wealth after retirement (lumpsum)  and
# #    increase consumption after retirement (higher annuity) that then will be decreasing (self-managed pension payments will also decrease)
# out4 <- NULL; vec <- seq(0.1,1,0.1)
# for (nu3 in vec){
#   out <- totalCF(ret_age=65,c_age=42,
#                  w3=setNames(c(.25,.25,.25,.25,0),c("msci","b10","recom","libor","infl")),
#                  c=0.6,c2=0.12,nu2=.5,nu3=nu3,gender=0,
#                  w0=300000,li=100000,lg=0.01,c1=0.07,s1=c(15,80000),s2=300000,s3=300000,
#                  w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
#                  rho2=0.05,rho3=0.04,ret=ret[,,1:10],retr=retr[,,1:10],psi=0.015,alpha=0.96)
#   out4 <- rbind(out4,c(mean(out$cons["64",]),mean(out$cons["65",]),
#                        mean(out$wealth_before_ret["64",]),mean(out$wealth_after_ret["65",])))
# }
# par(mfrow=c(4,1),oma=c(0,0,0,0),mar=c(1,2,2,1))
# plot(vec,out4[,1],main = "Consumption before retirement")
# plot(vec,out4[,2],main = "Consumption after retirement")
# plot(vec,out4[,3],main = "Wealth before retirement")
# plot(vec,out4[,4],main = "Wealth at retirement")
# # 6) Higher illiquid wealth will increase taxes and therefore reduce consumption and wealth before and after retirement
# out4 <- NULL; vec <- seq(0,1000000,10000)
# for (w0 in vec){
#   out <- totalCF(ret_age=65,c_age=42,
#                  w3=setNames(c(.25,.25,.25,.25,0),c("msci","b10","recom","libor","infl")),
#                  c=0.6,c2=0.12,nu2=.5,nu3=0.01,gender=0,
#                  w0=w0,li=100000,lg=0.01,c1=0.07,s1=c(15,80000),s2=300000,s3=300000,
#                  w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
#                  rho2=0.05,rho3=0.04,ret=ret[,,1:10],retr=retr[,,1:10],psi=0.015,alpha=0.96)
#   out4 <- rbind(out4,c(mean(out$cons["64",]),mean(out$cons["65",]),
#                        mean(out$wealth_before_ret["64",]),mean(out$wealth_after_ret["65",])))
# }
# par(mfrow=c(4,1),oma=c(0,0,0,0),mar=c(1,2,2,1))
# plot(vec,out4[,1],main = "Consumption before retirement")
# plot(vec,out4[,2],main = "Consumption after retirement")
# plot(vec,out4[,3],main = "Wealth before retirement")
# plot(vec,out4[,4],main = "Wealth at retirement")
# # 6) Higher liquid wealth (third pillar) will increase taxes and therefore reduce consumption before retirement but
# #    will increase wealth before and after retirement and therefore consumption after retirement
# out4 <- NULL; vec <- seq(0,1000000,10000)
# for (s3 in vec){
#   out <- totalCF(ret_age=65,c_age=42,
#                  w3=setNames(c(.25,.25,.25,.25,0),c("msci","b10","recom","libor","infl")),
#                  c=0.6,c2=0.12,nu2=.5,nu3=0.01,gender=0,
#                  w0=300000,li=100000,lg=0.01,c1=0.07,s1=c(15,80000),s2=300000,s3=s3,
#                  w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
#                  rho2=0.05,rho3=0.04,ret=ret[,,1:10],retr=retr[,,1:10],psi=0.015,alpha=0.96)
#   out4 <- rbind(out4,c(mean(out$cons["64",]),mean(out$cons["65",]),
#                        mean(out$wealth_before_ret["64",]),mean(out$wealth_after_ret["65",])))
# }
# par(mfrow=c(4,1),oma=c(0,0,0,0),mar=c(1,2,2,1))
# plot(vec,out4[,1],main = "Consumption before retirement")
# plot(vec,out4[,2],main = "Consumption after retirement")
# plot(vec,out4[,3],main = "Wealth before retirement")
# plot(vec,out4[,4],main = "Wealth at retirement")
# # 7) Higher wealth in the second pillar will do nothing to wealth and consumption before retirement but
# #    will increase wealth and therefore consumption after retirement
# out4 <- NULL; vec <- seq(0,1000000,10000)
# for (s2 in vec){
#   out <- totalCF(ret_age=65,c_age=42,
#                  w3=setNames(c(.25,.25,.25,.25,0),c("msci","b10","recom","libor","infl")),
#                  c=0.6,c2=0.12,nu2=.5,nu3=0.01,gender=0,
#                  w0=300000,li=100000,lg=0.01,c1=0.07,s1=c(15,80000),s2=s2,s3=300000,
#                  w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
#                  rho2=0.05,rho3=0.04,ret=ret[,,1:10],retr=retr[,,1:10],psi=0.015,alpha=0.96)
#   out4 <- rbind(out4,c(mean(out$cons["64",]),mean(out$cons["65",]),
#                        mean(out$wealth_before_ret["64",]),mean(out$wealth_after_ret["65",])))
# }
# par(mfrow=c(4,1),oma=c(0,0,0,0),mar=c(1,2,2,1))
# plot(vec,out4[,1],main = "Consumption before retirement")
# plot(vec,out4[,2],main = "Consumption after retirement")
# plot(vec,out4[,3],main = "Wealth before retirement")
# plot(vec,out4[,4],main = "Wealth at retirement")
# # 8) Higher wealth in the first pillar will do nothing to wealth and consumption before retirement but
# #    will increase consumption after retirement
# ## here one has to decrease li to 10000 or the "brackets" take over
# out4 <- NULL; vec <- seq(10000,100000,1000)
# for (s12 in vec){
#   out <- totalCF(ret_age=65,c_age=42,
#                  w3=setNames(c(.25,.25,.25,.25,0),c("msci","b10","recom","libor","infl")),
#                  c=0.6,c2=0.12,nu2=.5,nu3=0.01,gender=0,
#                  w0=300000,li=10000,lg=0.01,c1=0.07,s1=c(15,s12),s2=300000,s3=300000,
#                  w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
#                  rho2=0.05,rho3=0.04,ret=ret[,,1:10],retr=retr[,,1:10],psi=0.015,alpha=0.96)
#   out4 <- rbind(out4,c(mean(out$cons["64",]),mean(out$cons["65",]),
#                        mean(out$wealth_before_ret["64",]),mean(out$wealth_after_ret["65",])))
# }
# par(mfrow=c(4,1),oma=c(0,0,0,0),mar=c(1,2,2,1))
# plot(vec,out4[,1],main = "Consumption before retirement")
# plot(vec,out4[,2],main = "Consumption after retirement")
# plot(vec,out4[,3],main = "Wealth before retirement")
# plot(vec,out4[,4],main = "Wealth at retirement")
#  # 9) Increasing pension (consumption) in higher conversion factors rho2/rho3
# out4 <- NULL; vec <- seq(0.01,0.2,0.01)
# for (rho3 in vec){
#   out <- totalCF(ret_age=65,c_age=42,
#                  w3=setNames(c(.25,.25,.25,.25,0),c("msci","b10","recom","libor","infl")),
#                  c=0.6,c2=0.12,nu2=.5,nu3=0.01,gender=0,
#                  w0=300000,li=100000,lg=0.01,c1=0.07,s1=c(15,80000),s2=300000,s3=300000,
#                  w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
#                  rho2=0.05,rho3=rho3,ret=ret[,,1:10],retr=retr[,,1:10],psi=0.015,alpha=0.96)
#   out4 <- rbind(out4,c(mean(out$cons["64",]),mean(out$cons["65",]),
#                        mean(out$wealth_before_ret["64",]),mean(out$wealth_after_ret["65",])))
# }
# par(mfrow=c(4,1),oma=c(0,0,0,0),mar=c(1,2,2,1))
# plot(vec,out4[,1],main = "Consumption before retirement")
# plot(vec,out4[,2],main = "Consumption after retirement")
# plot(vec,out4[,3],main = "Wealth before retirement")
# plot(vec,out4[,4],main = "Wealth at retirement")
# # 10) Decreasing pension (consumption) for lower level of alpha (converting to a self-managed pension)
# out4 <- NULL; vec <- seq(1,0.8,-0.01)
# for (alpha in vec){
#   out <- totalCF(ret_age=65,c_age=42,
#                  w3=setNames(c(.25,.25,.25,.25,0),c("msci","b10","recom","libor","infl")),
#                  c=0.6,c2=0.12,nu2=.5,nu3=0.01,gender=0,
#                  w0=300000,li=100000,lg=0.01,c1=0.07,s1=c(15,80000),s2=300000,s3=300000,
#                  w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
#                  rho2=0.05,rho3=0.04,ret=ret[,,1:10],retr=retr[,,1:10],psi=0.015,alpha=alpha)
#   out4 <- rbind(out4,c(mean(out$cons["64",]),mean(out$cons["65",]),
#                        mean(out$wealth_before_ret["64",]),mean(out$wealth_after_ret["65",])))
# }
# par(mfrow=c(4,1),oma=c(0,0,0,0),mar=c(1,2,2,1))
# plot(vec,out4[,1],main = "Consumption before retirement")
# plot(vec,out4[,2],main = "Consumption after retirement")
# plot(vec,out4[,3],main = "Wealth before retirement")
# plot(vec,out4[,4],main = "Wealth at retirement")
