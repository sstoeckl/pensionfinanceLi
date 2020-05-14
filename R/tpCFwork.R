#' Third Pillar Cash Flows during savings phase
#'
#' Cash Flows are generated for the savings phase of the third pillar, starting at c_age until ret_age
#'
#' @param ret_age optional, retirement age, can be set anywhere between 60 and 70 (default: 65)
#' @param c_age the investor's current age (assuming birthday is calculation-day)
#' @param w3 third pillar portfolio allocation (given either as vector (constant investment or as matrix with
#' entries) for all remaining years
#' @param free_cf_before_tax remaining cash flow after contributing to first and second pillar
#' @param retr investment return scenarios (real)
#' @param s3 liquid wealth - invested in the third pillar (current assumption: no tax advantage for third pillar)
#' @param psi optional, spread to take a loan/leverage for third pillar savings
#' @param c fraction of income that is consumed while still working (current assumption: constant)
#' @param w0 time c_age wealth that is not disposable, assumption: still available at retirement (no growth or decline), alternatively: expected wealth (that is not disposable) at retirement
#' @param warnings optional: should warnings be given? (default=TRUE)
#'
#' @return list with two elemnts:
#' -  consumption during savings years (matrix with dim=c(sav_years,# of Scenarios))
#' -  Development of wealth during savings years (matrix with dim=c(sav_years,# of Scenarios))
#'
#' @examples
#' data(ret); data(retr)
#' free_cf_before_tax <- c(81000.00,81810.00,82628.10,83454.38,84288.92,85131.81,85983.13,86842.96,87711.39,88588.51,
#' 89474.39,90369.14,91272.83,92185.56,93107.41,94038.49,94978.87,95928.66,96887.95,97856.82,98835.39,99823.75,100821.98)
#' tpwork_ex <- tpCFwork(ret_age=65,c_age=42,w3=setNames(c(.25,.25,.25,.25,0),c("msci","b10","recom","libor","infl")),
#'            free_cf_before_tax=free_cf_before_tax,retr=retr[,,1:10],s3=300000,w0=300000,psi=0.015,c=0.6)
#' tpwork_ex2 <- tpCFwork(ret_age=65,c_age=42,w3=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
#'            free_cf_before_tax=free_cf_before_tax,retr=retr[,,1:10],s3=0,w0=0,psi=0.015,c=1)
#'
#' @export
tpCFwork<-function(ret_age=65, c_age, w3, free_cf_before_tax, retr, s3, w0, psi=0.015, c, warnings=TRUE){
  # control parameters w3=weights, retage = retirement age (same as in first pillar), annfrac = annuity fraction in [0,1]
  # s3 = savings3
  #########################################
  ## 0. checks
  if (warnings){
  if ((ret_age < 60)|ret_age>70) stop("'ret_age' must be between 60 and 70")
  if (c_age > ret_age) stop("'c_age' must be below 'ret_age'")
  if ((c_age < 18)|c_age>70) stop("'c_age' must be between 18 and 7")
  if (dim(retr)[1]!=122) stop("Somethings wrong with dimension of return vector")
  if (s3 < 0) warning("'s3' liquid wealth invested in third pillar is negative and will be punished with a larger LIBOR rate given by psi!")
  if ((psi < 0)|(psi > 1)) warning ("'psi' interest rate spread should be positive and not too large!")
  if (dim(retr)[2]!=length(w3)) stop("Somethings wrong with dimension of return vector vs the portfolio weights")
  if (!setequal(names(w3),colnames(ret[,,1]))) stop("The given portfolo weights do not match the given return names")
  if (abs(sum(w3)-1)>=10e-10) warning("'w3' Portfolio weights do not sum up to 1")
  if ((c < 0.1)|(c > 1)) warning ("'c' consumption while working should be between 0.1 and 1!")
  }
  #########################################
  ## 1. Pre-Calculations
  # years left for saving
  sav_years <- ret_age - c_age
  #########################################
  ## 2. Returns
  # check for leverage in the libor vector, add spread if negative
  if (w3["libor"] < 0){retr[,"libor",] <- retr[,"libor",] + psi}
  # portfolio returns (real) during saving years
  ma <- retr[c_age:(ret_age-1),names(w3),,drop=FALSE]
  ma_d <- retr[c_age:(ret_age-1),"libor",,drop=FALSE]
  pf_ret <- apply(ma,3,function(x) (exp(x)-1)%*%w3) # discrete returns times portfolio weights
  # interest for negative liquid wealth
  if (w3["libor"] < 0){debt <- apply(ma_d,3,function(x) (exp(x)-1))} else {debt <- apply(ma_d,3,function(x) (exp(x+psi)-1))}
  # necessary to keep matrix dimensions
    dim(pf_ret) <- c(length(c_age:(ret_age-1)),dim(retr)[3])
    dim(debt) <- c(length(c_age:(ret_age-1)),dim(retr)[3])
  # cf<-c(s3,freecfbeforetax)  #what for? (where used again?)
  #########################################
  ## 3. Wealth and Cash Flows
  wealth_development <- array(NA,c(ret_age-c_age,dim(retr)[3]))
  consumption <- array(NA,c(ret_age-c_age,dim(retr)[3]))
  rownames(wealth_development) <- rownames(consumption) <- as.character(seq(c_age,ret_age-1))
    # In first year
  cf_tax <- taxCFwork(free_cf_before_tax[1], s3, w0) # wealth from 1.1.
  free_cf_after_tax <- free_cf_before_tax[1] - cf_tax$from_cf
  # if s3<0: pay libor + psi. If s3 >0 invest according to w3/pf_ret
      wealth_development[1,] <- as.numeric(s3>0)*s3*(1+pf_ret[1,]) + as.numeric(s3<=0)*s3*(1+debt[1,]) + (1-c)*free_cf_after_tax - cf_tax$from_liquid_wealth
      consumption[1,] <- c*free_cf_after_tax
    # in all folowing years
  if(sav_years>=2){
    for(age in seq(c_age+1,ret_age-1)){
      t <- age - c_age
      # cash flow = income minus taxes (wealth from last period)
      cf_tax <- taxCFwork(free_cf_before_tax[t+1], wealth_development[t,], w0)
      free_cf_after_tax <- free_cf_before_tax[t+1] - cf_tax$from_cf
        wealth_development[t+1,] <- as.numeric(wealth_development[t,]>0)*wealth_development[t,]*(1+pf_ret[1,]) +
          as.numeric(wealth_development[t,]<=0)*wealth_development[t,]*(1+debt[1,]) + (1-c)*free_cf_after_tax - cf_tax$from_liquid_wealth
        consumption[t+1,] <- c*free_cf_after_tax
    }
  }
  tpw <- list()
    tpw$cons <- consumption
    tpw$wealth <- wealth_development
  return(tpw)
}
# ##
# # 1) Saving longer (higher retirement age) should increase wealth at retirement and keep consumption constant (due to higher wealth taxes, which we pay from liquid wealth)
# out4 <- NULL; vec <- 60:70
# for (ret_age in vec){
#   out <- tpCFwork(ret_age=ret_age,c_age=42,w3=setNames(c(.25,.25,.25,.25,0),c("msci","b10","recom","libor","infl")),
#                   free_cf_before_tax=rep(100000,ret_age-42),retr=retr[,,1:10],s3=300000,w0=300000,psi=0.015,c=0.6)
#   out4 <- rbind(out4,c(mean(out$cons[as.character(ret_age-1),]),mean(out$wealth[as.character(ret_age-1),])))
# }
# par(mfrow=c(2,1))
# plot(vec,out4[,1],main = "Consumption at retirement")
# plot(vec,out4[,2],main = "Wealth at retirement")
# # 2) Saving less years (higher current) should decrease wealth and keep consumption at retirement constant
# out4 <- NULL; vec <- 42:64
# for (c_age in vec){
#   out <- tpCFwork(ret_age=65,c_age=c_age,w3=setNames(c(.25,.25,.25,.25,0),c("msci","b10","recom","libor","infl")),
#                   free_cf_before_tax=rep(100000,ret_age-42),retr=retr[,,1:10],s3=300000,w0=300000,psi=0.015,c=0.6)
#   out4 <- rbind(out4,c(mean(out$cons["64",]),mean(out$wealth["64",])))
# }
# par(mfrow=c(2,1))
# plot(vec,out4[,1],main = "Consumption before retirement")
# plot(vec,out4[,2],main = "Wealth before retirement")
# # 3) Higher consumption should decrease wealth and increase consumption before retirement
# out4 <- NULL; vec <- seq(0.1,1,0.1)
# for (c in vec){
#   out <- tpCFwork(ret_age=65,c_age=42,w3=setNames(c(.25,.25,.25,.25,0),c("msci","b10","recom","libor","infl")),
#                   free_cf_before_tax=rep(100000,ret_age-42),retr=retr[,,1:10],s3=300000,w0=300000,psi=0.015,c=c)
#   out4 <- rbind(out4,c(mean(out$cons["64",]),mean(out$wealth["64",])))
# }
# par(mfrow=c(2,1))
# plot(vec,out4[,1],main = "Consumption before retirement")
# plot(vec,out4[,2],main = "Wealth before retirement")
# # 4) Higher illiquid wealth (w0) should increase taxes and therefore reduce wealth within certain brackets (consumption constant)
# out4 <- NULL; vec <- seq(0,1000000,10000)
# for (w0 in vec){
#   out <- tpCFwork(ret_age=65,c_age=42,w3=setNames(c(.25,.25,.25,.25,0),c("msci","b10","recom","libor","infl")),
#                   free_cf_before_tax=rep(100000,ret_age-42),retr=retr[,,1:10],s3=300000,w0=w0,psi=0.015,c=0.6)
#   out4 <- rbind(out4,c(mean(out$cons["64",]),mean(out$wealth["64",])))
# }
# par(mfrow=c(2,1))
# plot(vec,out4[,1],main = "Consumption before retirement")
# plot(vec,out4[,2],main = "Wealth before retirement")
# # 5) Higher liquid wealth (s3) should increase returns and also taxes but not reduce consumption
# out4 <- NULL; vec <- seq(0,1000000,10000)
# for (s3 in vec){
#   out <- tpCFwork(ret_age=65,c_age=42,w3=setNames(c(.25,.25,.25,.25,0),c("msci","b10","recom","libor","infl")),
#                   free_cf_before_tax=rep(100000,ret_age-42),retr=retr[,,1:10],s3=s3,w0=300000,psi=0.015,c=0.6)
#   out4 <- rbind(out4,c(mean(out$cons["64",]),mean(out$wealth["64",])))
# }
# par(mfrow=c(2,1))
# plot(vec,out4[,1],main = "Consumption before retirement")
# plot(vec,out4[,2],main = "Wealth before retirement")
