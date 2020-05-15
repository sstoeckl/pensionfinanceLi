#' Second Pillar Cash Flows
#'
#' Cash Flows are generated for the payment phase of the second pillar, starting at ret_age until 122 because that is when all our
#' ficticious persons have died. Contributions to the second pillar are doubled by the employer
#'
#'
#' @param ret_age optional, retirement age, can be set anywhere between 60 and 70 (default: 65)
#' @param nu2 fraction of second pillar savings that is converted to life-long pension
#' @param c_age the investor's current age (assuming birthday is calculation-day)
#' @param c2 second pillar savings as fraction of gross income (still missing: health, a-fonds-perdu payments)
#' @param li gross labor income at time 0 (in the last year before birthday)
#' @param lg labor growth rate (in real terms, constant)
#' @param w2 portfolio allocation (assumed to be fixed and not influenced by the decision maker)
#' @param ret investment return scenarios (nominal)
#' @param retr investment return scenarios (real)
#' @param s2 savings in second pillar as of t=0
#' @param rho2 conversion factor in second pillar for regular retirement age
#' @param warnings optional: should warnings be given? (default=TRUE)
#'
#' @return List with elements:
#' - a vector of lumpsum spendings (length = number of scenarios) and
#' - a matrix with annuity payments starting at ret_age until 122 (rows) with number of columns equal to the number of scenarios
#' - vector of wealth levels at retirement
#'
#' @examples
#' data(ret); data(retr)
#' sp_ex <- spCF(ret_age=65,nu2=.5,c_age=42,c2=.12,
#'            li=100000,lg=0.01,
#'            ret=ret[,,1:10],retr=retr[,,1:10],s2=300000,rho2=0.05)
#' sp_ex2 <- spCF(ret_age=65,nu2=0.01,c_age=64,c2=0.01,li=0,lg=0.01,w2=setNames(c(1,0,0,0,0),c("msci","b10","recom","libor","infl")),
#'            ret=ret[,,1:10],retr=retr[,,1:10],s2=0,rho2=0.0000001)
#'
#' @importFrom pracma ones
#' @importFrom stats setNames quantile
#'
#' @export
spCF <- function(ret_age=65,nu2,c_age,c2,li,lg,w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),ret,retr,s2,rho2, warnings=TRUE){
  # control parameters ret_age = retirement_age (as first pillar), annfrac = annuity fraction in [0,1]
  # s2 = savings2, rho2 = conversion factor
  # w2 = weights given
  # contributions and revenues within the pension savings account are tax free
  # payouts are taxed (different lump sum or annuity)
  # assumption: adjustments for early/late retirement as currently in SPL
  # assumption: no inflation adjustment of pensions in second pillar (!)
  # assumption: 50/50 contribution split employer/employee
  # interest is payed until the end of (ret_age-1), first pension at the end of ret_age, which is already inflation adjusted
  #########################################
  ## 0. checks
  if (warnings){
  if ((ret_age < 60)|ret_age>70) stop("'ret_age' must be between 60 and 70")
  if (c_age >= ret_age) stop("'c_age' must be below 'ret_age'")
  if ((c_age < 18)|c_age>70) stop("'c_age' must be between 18 and 7")
  if (li < 0) stop("'li' labor income must be larger than 0")
  if ((lg < 0)|(lg>1)) warning ("'lg' labor income growth values above 1 or below 0 can have unintended consequences")
  if (dim(ret)[1]!=122) stop("Somethings wrong with dimension of return vector")
  if (dim(retr)[1]!=122) stop("Somethings wrong with dimension of return vector")
  if (s2 < 0) stop("'s2' savings in second pillar must be larger than 0")
  if ((rho2 <= 0)|(rho2 > 0.2)) warning ("'rho2' conversion rate should be larger than zero and smaller than 0.2 (which is already an extreme case)")
  if ((nu2 < 0)|(nu2 > 1)) stop ("'nu2' lumpsum vs annuity decision must be between 0 and 1")
  if (dim(ret)[2]!=length(w2)) stop("Somethings wrong with dimension of return vector vs the portfolio weights")
  if (!setequal(names(w2),colnames(ret[,,1]))) stop("The given portfolio weights do not match the given return names")
  if (sum(w2)!=1) warning("'w2' Portfolio weights do not sum up to 1")
  }
  #########################################
  ## 1. Pre-Calculations
  # years left for saving
  sav_years <- ret_age - c_age
  #########################################
  ## 2. Returns
  # portfolio returns (real) during saving years
  ma <- retr[c_age:(ret_age-1),names(w2),,drop=FALSE]
  pf_ret <- apply(ma,3,function(x) (exp(x)-1)%*%w2) # discrete real returns times portfolio weights
    # necessary to keep matrix dimensions
    dim(pf_ret) <- c(length(c_age:(ret_age-1)),dim(ret)[3])
  # now limit max and min performance (equivalent to smoothed performance)
  # for all future periods and scenarios we limit
  ind25 <- which(pf_ret<quantile(pf_ret,.25))
  ind75 <- which(pf_ret>quantile(pf_ret,.75))
  # calculate actually paid returns as mean over all (smoothed)
  payed_ret <- pracma::ones(nrow(pf_ret),ncol(pf_ret))*mean(pf_ret)
  # the best/worst years get 2% more (less) than the overall mean
  payed_ret[ind25] <- mean(pf_ret)-.02
  payed_ret[ind75] <- mean(pf_ret)+.02
  rownames(payed_ret) <- as.character(c_age:(ret_age-1))
  #########################################
  ## 3. Cashflow
  # savings in second pillar (starting with savings until now, growing on with fraction of (growing) labor income * 2)
  lcf <- c(s2,(c2+min(c2,0.12))*li*(1+lg)^(seq(sav_years)-1))  # 2*c2 because of 50/50 contribution split
  # what is the wealth of each element of lcf at the end of savings phase?
  sp_wealth_at_ret_age <- apply(payed_ret,2,function(x){lcf%*%c(rev(cumprod(1+rev(x))),1)}) # rev to give s0 the aggregate returns
  dim(sp_wealth_at_ret_age)<-c(1,ncol(payed_ret)); rownames(sp_wealth_at_ret_age) <- (ret_age-1)
  # Now calculate cashflows after ret_age
  # lumpsum payment from wealth_at ret_age
  sp_lumpsum <- sp_wealth_at_ret_age*(1-nu2)
  # annuitization of rest including adjustment for early/late retirement
  sp_pensionstart <- sp_wealth_at_ret_age*nu2*rho2*(1+0.126*(ret_age-65)) #last term is early/late retirement adjustment of SPL
  # adjustment for inflation: simplifying assumption of "no inflation adjustment"
  # no inflation adjustment leads to deflation in real terms
  #infl <- ret[ret_age:122,"infl",]
  #h2 <- apply(infl,2,function(x) exp(-cumsum(x/2)))
  h2 <- apply(ret[ret_age:122,"infl",],2,function(x) exp(-cumsum(x/2)))
  rownames(h2) <- as.character(ret_age:122)
  sp_pension <- matrix(sp_pensionstart,byrow=TRUE,nrow=nrow(h2),ncol=ncol(h2))*h2
  # create output
  spw <- list()
  spw$lumpsum <- sp_lumpsum
  spw$pension <- sp_pension
  spw$wealth <- sp_wealth_at_ret_age
  return(spw)
}
# # Test cases
# # 1) sp should all be decreasing in rising age
# out1 <- NULL
# for (c_age in 42:64){
#   out <- spCF(ret_age=65,nu2=.5,c_age=c_age,c2=.12,li=100000,lg=0.01,ret=ret[,,1:10],retr=retr[,,1:10],s2=300000,rho2=0.05)
#   out1 <- rbind(out1,c(mean(out$lumpsum),mean(out$pension[1,]),mean(out$wealth)))
# }
# par(mfrow=c(3,1))
# plot(42:64,out1[,1],main = "Lumpsum")
# plot(42:64,out1[,2],main = "First pension")
# plot(42:64,out1[,3],main = "Wealth at retirement")
# # 2) sp should all be increasing in rising retirement age
# out2 <- NULL
# for (ret_age in 60:70){
#   out <- spCF(ret_age=ret_age,nu2=.5,c_age=42,c2=.12,li=100000,lg=0.01,ret=ret[,,1:10],retr=retr[,,1:10],s2=300000,rho2=0.05)
#   out2 <- rbind(out2,c(mean(out$lumpsum),mean(out$pension[1,]),mean(out$wealth)))
# }
# par(mfrow=c(3,1))
# plot(60:70,out2[,1],main = "Lumpsum")
# plot(60:70,out2[,2],main = "First pension")
# plot(60:70,out2[,3],main = "Wealth at retirement")
# # 3) decreasing lumpsum/increasing pension/same wealth in nu
# out3 <- NULL
# for (nu2 in seq(0,1,0.1)){
#   out <- spCF(ret_age=65,nu2=nu2,c_age=42,c2=.12,li=100000,lg=0.01,ret=ret[,,1:10],retr=retr[,,1:10],s2=300000,rho2=0.05)
#   out3 <- rbind(out3,c(mean(out$lumpsum),mean(out$pension[1,]),mean(out$wealth)))
# }
# par(mfrow=c(3,1))
# plot(seq(0,1,0.1),out3[,1],main = "Lumpsum")
# plot(seq(0,1,0.1),out3[,2],main = "First pension")
# plot(seq(0,1,0.1),out3[,3],main = "Wealth at retirement")
# # 4) increasing all in c2
# out4 <- NULL
# for (c2 in seq(0,1,0.1)){
#   out <- spCF(ret_age=65,nu2=0.5,c_age=42,c2=c2,li=100000,lg=0.01,ret=ret[,,1:10],retr=retr[,,1:10],s2=300000,rho2=0.05)
#   out4 <- rbind(out4,c(mean(out$lumpsum),mean(out$pension[1,]),mean(out$wealth)))
# }
# par(mfrow=c(3,1))
# plot(seq(0,1,0.1),out4[,1],main = "Lumpsum")
# plot(seq(0,1,0.1),out4[,2],main = "First pension")
# plot(seq(0,1,0.1),out4[,3],main = "Wealth at retirement")
# # 5) increasing all in li
# out4 <- NULL; vec <- seq(10000,100000,10000)
# for (li in vec){
#   out <- spCF(ret_age=65,nu2=0.5,c_age=42,c2=.12,li=li,lg=0.01,ret=ret[,,1:10],retr=retr[,,1:10],s2=300000,rho2=0.05)
#   out4 <- rbind(out4,c(mean(out$lumpsum),mean(out$pension[1,]),mean(out$wealth)))
# }
# par(mfrow=c(3,1))
# plot(vec,out4[,1],main = "Lumpsum")
# plot(vec,out4[,2],main = "First pension")
# plot(vec,out4[,3],main = "Wealth at retirement")
# # 6) increasing all in lg
# out4 <- NULL; vec <- seq(0,0.5,0.05)
# for (lg in vec){
#   out <- spCF(ret_age=65,nu2=0.5,c_age=42,c2=.12,li=100000,lg=lg,ret=ret[,,1:10],retr=retr[,,1:10],s2=300000,rho2=0.05)
#   out4 <- rbind(out4,c(mean(out$lumpsum),mean(out$pension[1,]),mean(out$wealth)))
# }
# par(mfrow=c(3,1))
# plot(vec,out4[,1],main = "Lumpsum")
# plot(vec,out4[,2],main = "First pension")
# plot(vec,out4[,3],main = "Wealth at retirement")
# # 7) increasing all in s2
# out4 <- NULL; vec <- seq(10000,1000000,10000)
# for (s2 in vec){
#   out <- spCF(ret_age=65,nu2=0.5,c_age=42,c2=.12,li=100000,lg=0.01,ret=ret[,,1:10],retr=retr[,,1:10],s2=s2,rho2=0.05)
#   out4 <- rbind(out4,c(mean(out$lumpsum),mean(out$pension[1,]),mean(out$wealth)))
# }
# par(mfrow=c(3,1))
# plot(vec,out4[,1],main = "Lumpsum")
# plot(vec,out4[,2],main = "First pension")
# plot(vec,out4[,3],main = "Wealth at retirement")
# # 8) increasing pension in conversion factor rho2
# out4 <- NULL; vec <- seq(0.01,0.2,0.01)
# for (rho2 in vec){
#   out <- spCF(ret_age=65,nu2=0.5,c_age=42,c2=.12,li=100000,lg=0.01,ret=ret[,,1:10],retr=retr[,,1:10],s2=300000,rho2=rho2)
#   out4 <- rbind(out4,c(mean(out$lumpsum),mean(out$pension[1,]),mean(out$wealth)))
# }
# par(mfrow=c(3,1))
# plot(vec,out4[,1],main = "Lumpsum")
# plot(vec,out4[,2],main = "First pension")
# plot(vec,out4[,3],main = "Wealth at retirement")
