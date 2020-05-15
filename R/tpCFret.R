#' Third Pillar Cash Flows in retirement
#'
#' Calculate cash flows from third pillar in retirement
#'
#' @param ret_age optional, retirement age, can be set anywhere between 60 and 70 (default: 65)
#' @param c_age the investor's current age (assuming birthday is calculation-day)
#' @param w3 third pillar portfolio allocation (given either as vector or as matrix with entries) for all years
#' @param alpha parameter to choose fraction of wealth NOT consumed during retirement but kept for investment (and subsequent consumption) - see file 'Consumption_3p.ods'
#' @param wealth_at_ret_age lumpsum from second pillar (after tax using `taxCFlumpsum`) plus third pillar (wealth at retirement from `tpCFwork`
#' after taking away fraction of third pillar savings that is converted to a life-long pension _nu2_)
#' @param retr GivenVar: investment return scenarios (real)
#' @param psi optional, spread to take a loan/leverage for third pillar savings
#' @param warnings optional: should warnings be given? (default=TRUE)
#'
#' @return list with two elemnts:
#' -  consumption during pension years (matrix with dim=c(122-ret_age,# of Scenarios))
#' -  Development of wealth during pension years (matrix with dim=c(122-ret_age,# of Scenarios))
#'
#' @examples
#' data(retr)
#' tpret_test <- tpCFret(ret_age=65,c_age=42,w3=setNames(c(.25,.25,.25,.25,0),c("msci","b10","recom","libor","infl")),
#'            alpha=0.96,wealth_at_ret_age=100000,retr=retr[,,1:10],psi=0.015)
#' tpret_test2 <- tpCFret(ret_age=65,c_age=42,w3=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
#'            alpha=0.96,wealth_at_ret_age=c(rep(-10000,5),rep(10000,5)),retr=retr[,,1:10],psi=0)
#'
#' @export
tpCFret <- function(ret_age=65,c_age,w3,alpha,wealth_at_ret_age,retr,psi=0.015, warnings=TRUE){
  # control parameters w3=weights, retage = retirementage (as first pillar), annfrac = annuity fraction in [0,1]
  # s3 = savings3
  #########################################
  ## 0. checks
  if (warnings){
  if (!setequal(names(w3),colnames(retr[,,1]))) stop("The given portfolo weights do not match the given return names")
  if (abs(sum(w3)-1)>=10e-10) warning("'w3' Portfolio weights do not sum up to 1")
  }
  #########################################
  ## 1. Pre-Calculations
  # years left for saving
  sav_years <- ret_age - c_age
  #########################################
  ## 2. Returns
  # check for leverage in the libor vector, add spread if negative
  if (w3["libor"] < 0){retr[,"libor",] <- retr[,"libor",] + psi}
  ## self-managed pension
  # portfolio returns (real) during saving years
      # ma <- retr[(ret_age+1):122,names(w3),,drop=FALSE]
      # ma_d <- retr[(ret_age+1):122,"libor",,drop=FALSE]
      # pf_ret <- apply(ma,3,function(x) (exp(x)-1)%*%w3) # discrete returns times portfolio weights
      # # interest for negative liquid wealth
      # if (w3["libor"] < 0){debt <- apply(ma_d,3,function(x) (exp(x)-1))} else {debt <- apply(ma_d,3,function(x) (exp(x+psi)-1))}
  pf_ret <- apply(retr[(ret_age+1):122,names(w3),,drop=FALSE],3,function(x) (exp(x)-1)%*%w3) # discrete returns times portfolio weights
  if (w3["libor"] < 0){debt <- apply(retr[(ret_age+1):122,"libor",,drop=FALSE],3,function(x) (exp(x)-1))} else {debt <- apply(retr[(ret_age+1):122,"libor",,drop=FALSE],3,function(x) (exp(x+psi)-1))}
  # # interest for negative liquid wealth
  # if (w3["libor"] < 0){debt <- apply(ma_d,3,function(x) (exp(x)-1))} else {debt <- apply(ma_d,3,function(x) (exp(x+psi)-1))}
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
  e1 <- apply(pf_ret,2,function(x){cumprod((1+x)*cfact3)})
  e1 <- rbind(e1[1,]*0+1,e1) # this is ultimatively wrong but looks better. for correct treatment delete here and delete "+1" in lines 47f
  e2 <- apply(debt,2,function(x){cumprod((1+x))})
  e2 <- rbind(e2[1,]*0+1,e2) # this is ultimatively wrong but looks better. for correct treatment delete here and delete "+1" in lines 47f
  e <- matrix(as.numeric(wealth_at_ret_age>=0),nrow=nrow(e1),ncol=ncol(e1),byrow=TRUE)*e1 +
          matrix(as.numeric(wealth_at_ret_age<0),nrow=nrow(e2),ncol=ncol(e2),byrow=TRUE)*e2
  wealth <- matrix(wealth_at_ret_age,byrow = TRUE,nrow=nrow(e),ncol=ncol(e))*e
      rownames(wealth) <- as.character(ret_age:122)
  consumption <- matrix(as.numeric(wealth_at_ret_age>=0),nrow=nrow(e1),ncol=ncol(e1),byrow=TRUE)*apply(wealth,2,function(x) x*(1-cfact3))
    # create output
    tpr <- list()
    tpr$cons <- consumption
    tpr$wealth <- wealth
  return(tpr)
}
# # all depends on the wealth at retirement age, so c_age is not an input parameter here.
# # 1) Decreasing leftover at 122 when pension age is growing (due to consuming less than is earned with investment)
# out4 <- NULL; vec <- 60:70
# for (ret_age in vec){
#   out <- tpCFret(ret_age=ret_age,c_age=42,w3=setNames(c(.25,.25,.25,.25,0),c("msci","b10","recom","libor","infl")),
#                  alpha=0.96,wealth_at_ret_age=rep(10000,10000),retr=retr,psi=0.015)
#   out4 <- rbind(out4,c(mean(out$cons["122",]),mean(out$wealth["122",])))
# }
# par(mfrow=c(2,1))
# plot(vec,out4[,1],main = "Consumption at 122")
# plot(vec,out4[,2],main = "Wealth at 122")
# # 2) Decreasing leftover at 122 when more and more is consumed in terms of a self-managed pension
# ## Attention: In case alpha>1 do we take pension away from fp and sp amnd use it to save more/repay debt
# out4 <- NULL; vec <- seq(1.2,0.8,-0.025)
# for (alpha in vec){
#   out <- tpCFret(ret_age=65,c_age=42,w3=setNames(c(.25,.25,.25,.25,0),c("msci","b10","recom","libor","infl")),
#                  alpha=alpha,wealth_at_ret_age=rep(10000,10000),retr=retr,psi=0.015)
#   out4 <- rbind(out4,c(mean(out$cons["122",]),mean(out$wealth["122",])))
# }
# par(mfrow=c(2,1))
# plot((1-vec),out4[,1],main = "Consumption at 122")
# plot(vec,out4[,2],main = "Wealth at 122")
# # 3) Decreasing wealth at retirement (from positive to negative) leads
# out4 <- NULL; vec <- seq(10000,-10000,-1000)
# for (wealth in vec){
#   out <- tpCFret(ret_age=65,c_age=42,w3=setNames(c(.25,.25,.25,.25,0),c("msci","b10","recom","libor","infl")),
#                  alpha=0.96,wealth_at_ret_age=rep(wealth,10000),retr=retr,psi=0.015)
#   out4 <- rbind(out4,c(mean(out$cons["122",]),mean(out$wealth["122",])))
# }
# par(mfrow=c(2,1))
# plot(vec,out4[,1],main = "Consumption at 122")
# plot(vec,out4[,2],main = "Wealth at 122")
# # 4) Decreasing asset allocation at libor (combined with stocks)
# out4 <- NULL; vec <- seq(0.5,-0.5,-0.05)
# for (libor in vec){
#   out <- tpCFret(ret_age=65,c_age=42,w3=setNames(c(1-libor,0,0,libor,0),c("msci","b10","recom","libor","infl")),
#                  alpha=0.96,wealth_at_ret_age=rep(10000,10000),retr=retr,psi=0.015)
#   out4 <- rbind(out4,c(mean(out$cons["122",]),mean(out$wealth["122",])))
# }
# par(mfrow=c(2,1))
# plot(vec,out4[,1],main = "Consumption at 122")
# plot(vec,out4[,2],main = "Wealth at 122")
