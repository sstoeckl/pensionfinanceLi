#' Simulate from given (pre-estimated) VAR process
#'
#' @param V Output of a VAR Estimation (in package tsDyn)
#' @param simN optional, number of simulated return series to generate, defaults to 1000
#' @param age.max optional,maximum age, defaults to 122
#' @param age optional,starting age, defaults to one, as we usually want to generate all lifetime returns and select from them accordingly
#' @param frequency optional,defaults to 4 (quarterly), could also be 12 (monthly)
#' @param s optional, (from VAR.sim): Starting values (matrix of dimension lag x k) for the VAR to simulate, in our case this
#' defaults to the steady state calculated as $s=(diag(k)-B)^{-1} * c, where k is the dimension of the VAR, B the coefficients
#' of parameters and equations and c the intercepts of the equations, all taken from V
#' @param covres optional, Variance-covariance matrix for the innovations, defaults to the covariance matrix of the VAR given by V as $var(V$residuals)/(V$T-2-V$k)*(V$T-2)$
#'
#' @return An array of dimension (T,k,simN) containing simN simulated return series for T years and k variables
#' @export
#'
#' @examples
#' data(V)
#' retPA <- VARsim(V,simN=10)
#'
#' @export
VARsim <- function(V, simN=1000, age.max=122, age=0, frequency=4, s=NULL, covres=NULL){
  T <- (age.max-age)*frequency # work with quarterly/monthly data (frequency = 4 or 12)
  # if starting is null, calculate steady state from model and use as starting
  if (is.null(s)){
    B <- V$coefficients[,-1]; c <- V$coefficients[,1]
    s <- solve(diag(V$k)-B)%*%c
  }
  # if covres is null, we calculate the covariance of the original residuals
  if (is.null(covres)){
    covres <- var(V$residuals)/(V$T-2-V$k)*(V$T-2)
  }
  # create empty array skeleton
  retPA <- array(NA,c(T/frequency,V$k,simN))
  y <- rep(seq(T/frequency),each=frequency)
  # create progress bar
  pb <- txtProgressBar(min = 1, max = simN, initial = 1, style=3)
  for(n in seq_len(simN)){
    setTxtProgressBar(pb,n)
    P <- tsDyn::VAR.sim(V$coefficients,n=T,lag=V$lag,include='const',starting=t(s),varcov = covres)
    # rebalancing on an annual basis - bring process on annual basis (sum four quarters)
    retPA[,,n] <- rowsum(P,y)
  }
  dimnames(retPA)[2] <- list(names(V$model[,1:V$k]))
  return(retPA)
}
