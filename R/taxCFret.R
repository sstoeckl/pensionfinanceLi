#' Calculate total taxes for pension cash-flows
#'
#' @param fpcf first pillar pension cash-flow
#' @param totalcf total pension cash-flow (including fpcf)
#' @param wealth wealth at 1.1. of tax year (can be negative)
#' @param warnings optional: should warnings be given? (default=TRUE)
#'
#' @return pension income after tax (vector or scalar)
#'
#' @examples
#' taxCFret(fpcf=15000,totalcf=25000,wealth=100000)
#' taxCFret(fpcf=c(15000,15000),totalcf=c(30000,30000),wealth=c(100000,-120000))
#'
#' @export
taxCFret <- function(fpcf,totalcf,wealth,warnings=TRUE){
  #########################################
  ## 0. checks
  if (warnings){
  if (any(fpcf>totalcf)) stop("'fpcf' must be smaller than 'totalcf'")
  }
  #########################################
  ## 1. Calculations
  #########################################
  ## 2. Tax Settings
  deduct <- 5300 # sum of deductibles is 5300 CHF/year (excl. second pillar/already exhausted)
  # taxbrackets
  tbr <- c(15000,0.01,150,
           20000,0.02,400,
           40000,0.01,400,
           70000,0.01,700,
           100000,0.01,1000,
           130000,0.005,650,
           160000,0.005,800,
           200000,0.01,2000)
  dim(tbr) <- c(3,8); tbr <- t(tbr)
  #########################################
  ## 3.
  taxbase <- totalcf + wealth*0.04 - deduct - fpcf*0.7  # special tax treatment of first pillar pension, Summe Freibeträge 5300 CHF/Jahr (exkl. "zweite Säule"/schon ausgeschöpft)
  taxbase_no_wealth <- totalcf - deduct - fpcf*0.7
  taxamount <- (( (taxbase>tbr[1,1])*(taxbase*tbr[1,2]-tbr[1,3]) )+
    ( (taxbase>tbr[2,1])*(taxbase*tbr[2,2]-tbr[2,3]) )+
    ( (taxbase>tbr[3,1])*(taxbase*tbr[3,2]-tbr[3,3]) )+
    ( (taxbase>tbr[4,1])*(taxbase*tbr[4,2]-tbr[4,3]) )+
    ( (taxbase>tbr[5,1])*(taxbase*tbr[5,2]-tbr[5,3]) )+
    ( (taxbase>tbr[6,1])*(taxbase*tbr[6,2]-tbr[6,3]) )+
    ( (taxbase>tbr[7,1])*(taxbase*tbr[7,2]-tbr[7,3]) )+
    ( (taxbase>tbr[8,1])*(taxbase*tbr[8,2]-tbr[8,3]) ))*2.5 # assumption: 150% municipality tax
  taxamount_no_wealth <- (( (taxbase_no_wealth>tbr[1,1])*(taxbase_no_wealth*tbr[1,2]-tbr[1,3]) )+
    ( (taxbase_no_wealth>tbr[2,1])*(taxbase_no_wealth*tbr[2,2]-tbr[2,3]) )+
    ( (taxbase_no_wealth>tbr[3,1])*(taxbase_no_wealth*tbr[3,2]-tbr[3,3]) )+
    ( (taxbase_no_wealth>tbr[4,1])*(taxbase_no_wealth*tbr[4,2]-tbr[4,3]) )+
    ( (taxbase_no_wealth>tbr[5,1])*(taxbase_no_wealth*tbr[5,2]-tbr[5,3]) )+
    ( (taxbase_no_wealth>tbr[6,1])*(taxbase_no_wealth*tbr[6,2]-tbr[6,3]) )+
    ( (taxbase_no_wealth>tbr[7,1])*(taxbase_no_wealth*tbr[7,2]-tbr[7,3]) )+
    ( (taxbase_no_wealth>tbr[8,1])*(taxbase_no_wealth*tbr[8,2]-tbr[8,3]) ))*2.5  # assumption: 150% municipality tax
  taxcf <-list()
  taxcf$from_cf <- taxamount_no_wealth
  taxcf$from_wealth <- taxamount - taxamount_no_wealth
  return(taxcf)
}
