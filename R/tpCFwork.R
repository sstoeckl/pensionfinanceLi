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
tpCFwork<-function(ret_age=65, c_age, w3, free_cf_before_tax, retr, s3, w0, psi=0.015, c){
  # control parameters w3=weights, retage = retirement age (same as in first pillar), annfrac = annuity fraction in [0,1]
  # other rho3 = conversion factor in pillar three (may be gender-specific)
  # s3 = savings3
  #########################################
  ## 0. checks
  if ((ret_age < 60)|ret_age>70) stop("'ret_age' must be between 60 and 70")
  if (c_age > ret_age) stop("'c_age' must be below 'ret_age'")
  if ((c_age < 18)|c_age>70) stop("'c_age' must be between 18 and 7")
  if (dim(retr)[1]!=122) stop("Somethings wrong with dimension of return vector")
  if (s3 < 0) stop("'s3' liquid wealth invested in third pillar must be larger than 0")
  if ((psi < 0)|(psi > 1)) warning ("'psi' interest rate spread should be positive and not too large!")
  if (dim(retr)[2]!=length(w3)) stop("Somethings wrong with dimension of return vector vs the portfolio weights")
  if (!setequal(names(w3),colnames(ret[,,1]))) stop("The given portfolo weights do not match the given return names")
  if (sum(w3)!=1) warning("'w3' Portfolio weights do not sum up to 1")
  if ((c <= 0.1)|(c > 1)) warning ("'c' consumption while working should be between 0.1 and 1!")
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
  pf_ret <- apply(ma,3,function(x) (exp(x)-1)%*%w3) # discrete returns times portfolio weights
  # necessary to keep matrix dimensions
    dim(pf_ret) <- c(length(c_age:(ret_age-1)),dim(retr)[3])
  # cf<-c(s3,freecfbeforetax)  #what for? (where used again?)
  #########################################
  ## 3. Wealth and Cash Flows
  wealth_development <- array(NA,c(ret_age-c_age,dim(retr)[3]))
  consumption <- array(NA,c(ret_age-c_age,dim(retr)[3]))
  rownames(wealth_development) <- rownames(consumption) <- as.character(seq(c_age,ret_age-1))
    # In first year
    free_cf_after_tax <- taxCFwork(free_cf_before_tax[1], s3+w0)
      wealth_development[1,] <- s3*(1+pf_ret[1,]) + (1-c)*free_cf_after_tax
      consumption[1,] <- c*free_cf_after_tax
    # in all folowing years
  if(ret_age-c_age>=2){
    for(age in seq(c_age+1,ret_age-1)){
      t <- age - c_age
      # cash flow = income minus taxes (wealth from last period)
      free_cf_after_tax <- taxCFwork(free_cf_before_tax[t+1], wealth_development[t,] + w0)
        wealth_development[t+1,] <- wealth_development[t,]*(1+pf_ret[t+1,]) + (1-c)*free_cf_after_tax
        consumption[t+1,] <- c*free_cf_after_tax
    }
  }
  tpw <- list()
    tpw$cons <- consumption
    tpw$wealth <- wealth_development
  return(tpw)
}

