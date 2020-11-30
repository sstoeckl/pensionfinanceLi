#' First Pillar Cash Flows
#'
#' Cash Flows are generated for the payment phase of the first pillar, starting in retirement (ret_age) until 122 because that is when all our
#' fictitious persons have died. We use "Russian" inflation adjustment for starting pension: average income is computed mixing nominal past and
#' real future income, brackets in pension table are not adjusted ==> running pension in real terms decreases by half the inflation rate
#'
#' @param ret_age optional, retirement age, can be set anywhere between 60 and 70 (default: 65)
#' @param c_age the investor's current age (assuming birthday is calculation-day)
#' @param li gross labor income at time 0 (so at the end of year t=0/age=c_age it increases to li*(1+lg))
#' @param lg labor growth rate (in real terms, constant)
#' @param s1 vector consisting of two components: c(number of contribution years at age=c_age,historical average yearly income until c_age)
#' @param ret investment return scenarios (nominal)
#' @param warnings optional: should warnings be given? (default=TRUE)
#'
#' @return Named matrix of cashflows as of pension starting age (ret_age) until age=122 (dim=c(122-retage,# return scenarios))
#'
#' @examples
#' data(ret)
#' fp_ex <- fpCF(ret_age=65,c_age=42,li=100000,lg=0.01,s1=c(15,80000),ret=ret[,,1:10])
#' fp_ex2 <- fpCF(ret_age=65,c_age=42,li=0,lg=0.01,s1=c(0,0),ret=ret[,,1:10])
#'
#' @export
fpCF <- function(ret_age = 65, c_age, li, lg, s1, ret, warnings=TRUE){
  #########################################
  ## 0. checks
  if (warnings){
  if ((ret_age < 60)|ret_age>70) stop("'ret_age' must be between 60 and 70")
  if (c_age >= ret_age) stop("'c_age' must be below 'ret_age'")
  if ((c_age < 18)|c_age>70) stop("'c_age' must be between 18 and 7")
  if (li < 0) stop("'li' labor income must be larger than 0")
  if ((lg < 0)|(lg>1)) warning ("'lg' labor income growth values above 1 or below 0 can have unintended consequences")
  if (length(s1)!=2) stop("'s1' needs exactly two input parameters")
  if (s1[1]>c_age) stop("it is impossible to have more contribution years than 'c_age'")
  if (s1[2] < 0) stop("'s1[2]' average historical yearly labor income must be larger than 0")
  if (dim(ret)[1]!=122) stop("Somethings wrong with dimension of return vector")
  }
  #########################################
  ## 1. Pre-Calculations
  # years left for saving
  sav_years <- ret_age - c_age
  # generate vector of labor income for remaining working years
  laborincome <- li*(1+lg)^(seq(sav_years))
  # calculate average of labor income in remaining working years
  avg_laborincome <- mean(laborincome)
  # weigh with average labor income before today
  # 2.1 is a "mysterious revaluation factor" used by AHV
  avg_laborincome <- (avg_laborincome*sav_years + s1[1]*s1[2])/(sav_years + s1[1])*2.1
  # Data
  pension_adj <- c(-0.218,-0.18,-0.14,-0.097,-0.05,0,0.045,0.093,0.144,0.201,0.261); names(pension_adj) <- 60:70
  #########################################
  ## 2. First-Pillar specifics
  # Adjust income that will be evalued by 1stP to lie within these brackets:
  avg_laborincome[avg_laborincome<13920] <- 13920
  avg_laborincome[avg_laborincome>83520] <- 83520
  # The following calculates the theoretical pension to be received given 44 (max) contribution years, it is an approximation based on an (?) Excel regression
  full_pension <- (815.4 + 0.02622247*avg_laborincome - 1.0193*10^(-7)*avg_laborincome^2)*13 # from regression in excel to replace AHV table
  # Adjust for fraction of contribution years (base and max: 44 years)
  yearfrac <- (s1[1]+sav_years)/44 #assumption that 44 contribution years
  yearfrac[yearfrac>1] <- 1
  # calculate actual 1stP pension at ret_age and adjust for late/early retirement
  fp_pensionstart <- yearfrac*full_pension*(1+pension_adj[as.character(ret_age)])
  #########################################
  ## 3. Adjustment for inflation
  # adjustment for inflation: simplifying assumption of "always half of observed inflation rate" (even if inflation is negative)
  # adjustment for only half the inflation leads to deflation in real terms
  infl <- ret[ret_age:122,"infl",]
  h2 <- apply(infl,2,function(x) exp(-cumsum(x/2)))
  rownames(h2) <- as.character(ret_age:122)
  # Now create final vector of 1st pillar pension cashflows
  return(drop(fp_pensionstart)*h2)
}
