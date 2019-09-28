#' Third Pillar Cash Flows in retirement
#'
#' Calculate cash flows from third pillar in retirement
#'
#' @param ret_age optional, retirement age, can be set anywhere between 60 and 70 (default: 65)
#' @param c_age the investor's current age (assuming birthday is calculation-day)
#' @param w3 third pillar portfolio allocation (given either as vector or as matrix with entries) for all years
#' @param alpha parameter to choose fraction of wealth NOT consumed during retirement - see file 'Consumption_3p.ods'
#' @param wealth_at_ret_age lumpsum from second pillar (after tax using `taxCFlumpsum`) plus third pillar (wealth at retirement from `tpCFwork`
#' after taking away fraction of third pillar savings that is converted to a life-long pension _nu2_)
#' @param rho3 GivenVar: conversion factor in third pillar for regular retirement age
#' @param retr GivenVar: investment return scenarios (real)
#' @param psi optional, spread to take a loan/leverage for third pillar savings
#'
#' @return list with two elemnts:
#' -  consumption during pension years (matrix with dim=c(122-ret_age,# of Scenarios))
#' -  Development of wealth during pension years (matrix with dim=c(122-ret_age,# of Scenarios))
#'
#' @examples
#' data(retr)
#' tpret_ex <- tpCFret(ret_age=65,c_age=42,w3=setNames(c(.25,.25,.25,.25,0),c("msci","b10","recom","libor","infl")),
#'            alpha=0.96,wealth_at_ret_age=100000,rho3=0.04,retr=retr[,,1:10],psi=0.015)
#' tpret_ex2 <- tpCFret(ret_age=65,c_age=42,w3=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
#'            alpha=0.96,wealth_at_ret_age=0,rho3=0.04,retr=retr[,,1:10],psi=0)
#'
#' @export
tpCFret <- function(ret_age=65,c_age,w3,alpha,wealth_at_ret_age,rho3,retr,psi=0.015){
  # control parameters w3=weights, retage = retirementage (as first pillar), annfrac = annuity fraction in [0,1]
  # other rho3 = conversion factor as in pillar two (also gender-specific)
  # s3 = savings3
  #########################################
  ## 0. checks
  if (!setequal(names(w3),colnames(retr[,,1]))) stop("The given portfolo weights do not match the given return names")
  if (sum(w3)!=1) warning("'w3' Portfolio weights do not sum up to 1")
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
  ma <- retr[ret_age:122,names(w3),,drop=FALSE]
  pf_ret <- apply(ma,3,function(x) (exp(x)-1)%*%w3) # discrete returns times portfolio weights
  #
  cfact3 <- alpha # !!! alpha = const!!!
  # wealth development when (1-alpha) is taken away from wealth for consumption
  e <- apply(pf_ret,2,function(x){cumprod(exp(x)*cfact3)})

  wealth <- c(wealth_at_ret_age)*e
    rownames(wealth) <- as.character(ret_age:122)
  #wealth <- rbind(wealth_at_ret_age, wealth)

  consumption <- apply(wealth,2,function(x) x*(1-cfact3))
  # create output
  tpr <- list()
  tpr$cons <- consumption
  tpr$wealth <- wealth
  return(tpr)
}
