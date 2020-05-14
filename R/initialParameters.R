#' Load Parameters
#'
#' @import MortalityTables
#'
#' @export
.load_parameters <<- function(type=1,gend=0){
  library(MortalityTables)
  MortalityTables::mortalityTables.load("Austria_Annuities")
  if (gend==0){
    gender_mortalityTable <<- MortalityTables::baseTable(AVOe2005R.male)
    gender <<- gend
    } else {
    gender_mortalityTable <<- MortalityTables::baseTable(AVOe2005R.female)
    gender <<- gend
  }
  if (type==2){
    # Good earner, large debt
    ret_age <<- 65             # standard retirement age
    w3 <<- setNames(c(.25,.25,.25,.25,0),c("msci","b10","recom","libor","infl")) # portfolio weights (mostly not used)
    cc <<- 0.6                  # consumption from labor income
    c2 <<- 0.12                # contribution to second pillar from labor income (doubled by employer up to 12%)
    nu2 <<- 0.5                # part of the second pillar transformed to lifelong pension
    nu3 <<- 0.25               # part of the third pillar transformed to lifelong pension
    ra <<- 4                   # risk aversion
    delta <<- 0.02             # time preference
    aalpha <<- 0.96             # Fraction of pension wealth not consumed (kept for reinvestment)
    bbeta <<- 0.75              # Bequest utility weight
    c_age <<- 40               # current age
    w0 <<- 300000              # non-liquid wealth (counts only for wealth tax)
    CF <<- 0                   # NOT IMPLEMENTED
    li <<- 100000              # labor income
    lg <<- 0.01                # labor income annual growth
    c1 <<- 0.07                # contribution to first pillar (mandatory)
    s1 <<- c(15, 40000)        #
    s11 <<- 15
    s12 <<- 20000
    s2 <<-  20000              # Second pillar savings
    s3 <<-  -200000            # liquid wealth
    w2 <<- setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl"))
    rho2 <<- 0.05              # Pension Conversion Factor second pillar
    rho3 <<- 0.04              # Pension Conversion Factor third pillar
    psi <<- 0.015              # punishment for negative s3

  } else if (type==3) {
    # Low income, younger almost no savings, wants to work longer
    ret_age <<- 69             # standard retirement age
    w3 <<- setNames(c(.25,.25,.25,.25,0),c("msci","b10","recom","libor","infl")) # portfolio weights (mostly not used)
    cc <<- 0.8                 # consumption from labor income
    c2 <<- 0.12                # contribution to second pillar from labor income (doubled by employer up to 12%)
    nu2 <<- 0.75               # part of the second pillar transformed to lifelong pension
    nu3 <<- 0.75               # part of the third pillar transformed to lifelong pension
    ra <<- 10                  # risk aversion
    delta <<- 0.02             # time preference
    aalpha <<- 0.90            # Fraction of pension wealth not consumed (kept for reinvestment)
    bbeta <<- 0.25             # Bequest utility weight
    c_age <<- 35               # current age
    w0 <<- 10000               # non-liquid wealth (counts only for wealth tax)
    CF <<- 0                   # NOT IMPLEMENTED
    li <<- 40000               # labor income
    lg <<- 0.01                # labor income annual growth
    c1 <<- 0.07                # contribution to first pillar (mandatory)
    s1 <<- c(10, 20000)        #
    s11 <<- 10
    s12 <<- 20000
    s2 <<- 10000               # Second pillar savings
    s3 <<- 1000                # liquid wealth
    w2 <<- setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl"))
    rho2 <<- 0.05              # Pension Conversion Factor second pillar
    rho3 <<- 0.04              # Pension Conversion Factor third pillar
    psi <<- 0.015              # punishment for negative s3
  } else {
    # Standard Type
    ret_age <<- 65             #x standard retirement age
    w3 <<- setNames(c(.25,.25,.25,.25,0),c("msci","b10","recom","libor","infl")) # portfolio weights (mostly not used)
    cc <<- 0.75                # consumption from labor income
    c2 <<- 0.12                #x contribution to second pillar from labor income (doubled by employer up to 12%)
    nu2 <<- 0.94               # part of the second pillar transformed to lifelong pension
    nu3 <<- 0                  #-- part of the third pillar transformed to lifelong pension - according to Excel of Michael
    ra <<- 8                   #x risk aversion
    delta <<- 0.02             #x time preference
    aalpha <<- 0.96            # Fraction of pension wealth not consumed (kept for reinvestment)
    bbeta <<- 0.75             #x Bequest utility weight
    c_age <<- 50               #x current age
    w0 <<- 300000              #x non-liquid wealth (counts only for wealth tax)
    CF <<- 0                   # NOT IMPLEMENTED
    li <<- 100000              #x labor income
    lg <<- 0.01                #x labor income annual growth
    c1 <<- 0.07                #x contribution to first pillar (mandatory)
    s1 <<- c(20, 70000)        #x
    s11 <<- 20                 #x
    s12 <<- 70000              #x
    s2 <<- 300000              #x Second pillar savings
    s3 <<- 0                   #x liquid wealth
    w2 <<- setNames(c(.30,.30,.30,.10,0),
                    c("msci","b10","recom","libor","infl")) # x fixed eights of second pillar investment
    rho2 <<- 0.05              #x Pension Conversion Factor second pillar
    rho3 <<- 0.04              #x Pension Conversion Factor third pillar
    psi <<- 0.015              #x punishment for negative s3
  }
}
