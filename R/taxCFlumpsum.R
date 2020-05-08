#' Calculate lumpsum after taxes
#'
#' This function is supposed to subtract taxes on lumpsum payments from the second pillar at pension start
#'
#' @param lumpsum capital taken out at retirement from second pillar (vector?)
#' @param ret_age optional, retirement age, can be set anywhere between 60 and 70 (default: 65)
#' @param gender gender, 0=male and 1=female
#' @param warnings optional: should warnings be given? (default=TRUE)
#'
#' @return
#'
#' @examples
#' taxCFlumpsum(lumpsum=1000000,ret_age = c(60,64,65),gender=0)
#' taxCFlumpsum(lumpsum=1000000,ret_age = c(60,64,65),gender=c(0,1))
#' taxCFlumpsum(lumpsum=c(0,210,300),ret_age = 64,gender=0)
#'
#' @export
taxCFlumpsum <- function(lumpsum,ret_age = 65,gender,warnings=TRUE){
  #########################################
  ## 0. checks
  # ret_age and c_age checked before
  if (warnings){
  if (any((ret_age < 60)|ret_age>70)) stop("'ret_age' must be between 60 and 70")
  if (!all(unique(as.character(gender)) %in% c("0","1"))) stop("'gender' must be 0 or 1 (numeric or character)")
  }
  #########################################
  ## 1. Calculations
  #########################################
  ## 2. Tax Settings
  # taxbrackets
  tbr <- c(0,0.01,0,
           20000,0.02,400,
           40000,0.01,400,
           70000,0.01,700,
           100000,0.01,1000,
           130000,0.005,650,
           160000,0.005,800,
           200000,0.01,2000)
  dim(tbr) <- c(3,8); tbr <- t(tbr)
  # fictitious annual rent for tax purposes, sheet "Rentensatz mit Pro..." in "FNP Kapitalleistung...xls"
  annrate <- rbind( c(43.78,45.00,46.30,47.69,49.18,50.77,52.48,54.32,56.29,58.42,60.71),
                    c(40.84,41.85,42.93,44.09,45.33,46.67,48.12,49.68,51.38,53.21,55.21))
  colnames(annrate) <- as.character(60:70)
  rownames(annrate) <- as.character(c(0,1))
  #########################################
  ## 3.
  taxbase <- lumpsum
  # fictitious annual rent for tax purposes, sheet "Rentensatz mit Pro..." in "FNP Kapitalleistung...xls" cell Q19 and T41
  ann_rent <- floor(round(taxbase*annrate[as.character(gender),as.character(ret_age)]/1000*2,0)/2/10)*10
  tax_on_ann_rent <- ( (ann_rent>tbr[1,1])*(ann_rent*tbr[1,2]-tbr[1,3]) ) + # programmed as marginal tax rate per bracket
    ( (ann_rent>tbr[2,1])*(ann_rent*tbr[2,2]-tbr[2,3]) ) +
    ( (ann_rent>tbr[3,1])*(ann_rent*tbr[3,2]-tbr[3,3]) ) +
    ( (ann_rent>tbr[4,1])*(ann_rent*tbr[4,2]-tbr[4,3]) ) +
    ( (ann_rent>tbr[5,1])*(ann_rent*tbr[5,2]-tbr[5,3]) ) +
    ( (ann_rent>tbr[6,1])*(ann_rent*tbr[6,2]-tbr[6,3]) ) +
    ( (ann_rent>tbr[7,1])*(ann_rent*tbr[7,2]-tbr[7,3]) ) +
    ( (ann_rent>tbr[8,1])*(ann_rent*tbr[8,2]-tbr[8,3]) )
  taxfactor <- tax_on_ann_rent/ann_rent
  taxamount <- round(taxbase*taxfactor*2.5,1)
  taxamount[which(is.nan(taxamount))] <- 0
  return(lumpsum - taxamount)
}
