#' Various financial variables
#'
#' Data for estimating a VAR-process for Switzerland, necessary for optimizing over different asset classes
#'
#' @format A time series (xts) object with 588 rows and 8 variables (all described in the vignette `vignette("INPUT_returns")`)
#' @source THOMSON REUTERS DATASTREAM
"FINDATA"

#' VAR object for FINDATA
#'
#' The output of a VAR estimation using `lineVAR` from `tsDyn`.
#'
#' @format An object (list) with many elements
#'
#' @source own calculation
"V"

#' Asset returns (nominal) created internally
#'
#' 10000 return matrices simulated by VARsim using V
#'
#' @format An 121x5x10000 array
#'
#' @source own calculation
"ret"

#' Asset returns (real) created internally
#'
#' correct returns by subtracting the inflation column.
#'
#' @format An 121x5x10000 array
#'
#' @source own calculation
"retr"
