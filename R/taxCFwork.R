#' Calculate Income after taxes during savings phase
#'
#' The current function calculates income after deduction taxes during the cash-flow phase and is an important
#' input parameter for the third pillar function (tpCFwork), where CF after tax is neded to determine consumption/saving
#'
#' @param income income bfore tax (vector or scalar)
#' @param wealth wealth at 1.1. of tax year
#'
#' @return income after tax (vector or scalar)
#'
#' @examples
#' taxCFwork(income=c(100000,100000),wealth=c(10000,300000))
#'
#' @export
taxCFwork <- function(income,wealth){
  #########################################
  ## 0. checks
  # ret_age and c_age checked before
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
  taxbase <- income + wealth*0.04 - deduct
  taxbase_no_wealth <- income - deduct
  taxamount <- (( (taxbase>tbr[1,1])*(taxbase*tbr[1,2]-tbr[1,3]) )+
    ( (taxbase>tbr[2,1])*(taxbase*tbr[2,2]-tbr[2,3]) )+
    ( (taxbase>tbr[3,1])*(taxbase*tbr[3,2]-tbr[3,3]) )+
    ( (taxbase>tbr[4,1])*(taxbase*tbr[4,2]-tbr[4,3]) )+
    ( (taxbase>tbr[5,1])*(taxbase*tbr[5,2]-tbr[5,3]) )+
    ( (taxbase>tbr[6,1])*(taxbase*tbr[6,2]-tbr[6,3]) )+
    ( (taxbase>tbr[7,1])*(taxbase*tbr[7,2]-tbr[7,3]) )+
    ( (taxbase>tbr[8,1])*(taxbase*tbr[8,2]-tbr[8,3]) )) * 2.5 # assumption: 150% municipality tax
  taxamount_no_wealth <- (( (taxbase_no_wealth>tbr[1,1])*(taxbase_no_wealth*tbr[1,2]-tbr[1,3]) )+
                  ( (taxbase_no_wealth>tbr[2,1])*(taxbase_no_wealth*tbr[2,2]-tbr[2,3]) )+
                  ( (taxbase_no_wealth>tbr[3,1])*(taxbase_no_wealth*tbr[3,2]-tbr[3,3]) )+
                  ( (taxbase_no_wealth>tbr[4,1])*(taxbase_no_wealth*tbr[4,2]-tbr[4,3]) )+
                  ( (taxbase_no_wealth>tbr[5,1])*(taxbase_no_wealth*tbr[5,2]-tbr[5,3]) )+
                  ( (taxbase_no_wealth>tbr[6,1])*(taxbase_no_wealth*tbr[6,2]-tbr[6,3]) )+
                  ( (taxbase_no_wealth>tbr[7,1])*(taxbase_no_wealth*tbr[7,2]-tbr[7,3]) )+
                  ( (taxbase_no_wealth>tbr[8,1])*(taxbase_no_wealth*tbr[8,2]-tbr[8,3]) )) * 2.5 # assumption: 150% municipality tax
  taxcf <-list()
  taxcf$from_cf <- taxamount_no_wealth
  taxcf$from_wealth <- taxamount - taxamount_no_wealth
  return(taxcf)
}
