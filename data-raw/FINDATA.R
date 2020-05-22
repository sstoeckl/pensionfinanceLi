############################################################
### Create Return Vectors (function and simulation)
############################################################
## 0: Load Packages
library(tidyverse)
library(tsDyn)
library(PerformanceAnalytics)
library(pracma)
#library(RDatastream)
# user <- list(username = "", password = "")
############################################################
## 1: Download Data
## MSCI World (monthly, MSRI in CHF)
# dat <- ds(user, requests = "MSWRLD$(MSRI)~~CHF~1970-01-01~:2018-12-31~M")
# df<-as.data.frame(dat['Data',1])
# msci<-xts(df$P,order.by = df$DATE)
## FTSE Eurofirst 300 Switzerland S (monthly, RI in EUR, in CHF)
# dat <- ds(user, requests = "FTECHFS(RI)~~CHF~1970-01-01~:2018-12-31~M")
# df<-as.data.frame(dat['Data',1])
# fte<-xts(df$P,order.by = df$DATE)
## Swiss Market Index SMI (monthly, RI, in CHF)
# dat <- ds(user, requests = "SWISSMI(RI)~~CHF~1970-01-01~:2018-12-31~M")
# df<-as.data.frame(dat['Data',1])
# smi<-xts(df$P,order.by = df$DATE)
## SW Total 1-3 Years Datastream Government Index (monthly, RI in CHF)
# dat <- ds(user, requests = "ASWGVG1(RI)~~CHF~1970-01-01~:2018-12-31~M")
# df<-as.data.frame(dat['Data',1])
# b3<-xts(df$P,order.by = df$DATE)
## SW Total 7-10 Years Datastream Government Index (monthly, RI in CHF)
# dat <- ds(user, requests = "ASWGVG4(RI)~~CHF~1970-01-01~:2018-12-31~M")
# df<-as.data.frame(dat['Data',1])
# b10<-xts(df$P,order.by = df$DATE)
## IBA Swiss Franc Interbank LIBOR 3 Months Delayed (monthly, IO?)
# dat<- ds(user, requests = "BBCHF3M~1970-01-01~:2018-12-31~M")
# df<-as.data.frame(dat['Data',1])
# libor<-xts(df$P,order.by = df$DATE)
## Switzerland, Consumer Prices, Total, Index (monthly, index)
# dat<- ds(user, requests = "SWCONPRCF~1970-01-01~:2018-12-31~M")
# df<-as.data.frame(dat['Data',1])
# df$DATE<-make_date(year=year(df$DATE),month = month(df$DATE),day=1)
# infl<-xts(df$P,order.by = df$DATE)
## Switzerland, Real Estate Prices, Rented Properties, Rental Housing Units, Ask, Index (monthly, index)
# dat <- ds(user, requests = "SWHPIRATF~1970-01-01~:2018-12-31~M")
# df<-as.data.frame(dat['Data',1])
# df$DATE<-make_date(year=year(df$DATE),month = month(df$DATE),day=1)
# reres<-xts(df$P,order.by = df$DATE)
## Switzerland, Real Estate Prices, Rented Properties, Office Space, Ask, Index (monthly, index)
# dat <- ds(user, requests = "SWHPIOFFF~1970-01-01~:2018-12-31~M")
# df<-as.data.frame(dat['Data',1])
# df$DATE<-make_date(year=year(df$DATE),month = month(df$DATE),day=1)
# recom<-xts(df$P,order.by = df$DATE)
##################################################################
## 1B: Use EXCEL-INput File
msci <- readxl::read_xlsx("data-raw/Datastream_Switzerland_Assets.xlsx",sheet = "msci") %>%
  rename(DATE=Name,msci=!!names(.[2])) %>%
  mutate(DATE=as.yearmon(as.Date(DATE)),msci=as.numeric(msci))
fte <- readxl::read_xlsx("data-raw/Datastream_Switzerland_Assets.xlsx",sheet = "fte") %>%
  rename(DATE=Name,fte=!!names(.[2])) %>%
  mutate(DATE=as.yearmon(as.Date(DATE)),fte=as.numeric(fte))
smi <- readxl::read_xlsx("data-raw/Datastream_Switzerland_Assets.xlsx",sheet = "smi") %>%
  rename(DATE=Name,smi=!!names(.[2])) %>%
  mutate(DATE=as.yearmon(as.Date(DATE)),smi=as.numeric(smi))
b3 <- readxl::read_xlsx("data-raw/Datastream_Switzerland_Assets.xlsx",sheet = "b3") %>%
  rename(DATE=Name,b3=!!names(.[2])) %>%
  mutate(DATE=as.yearmon(as.Date(DATE)),b3=as.numeric(b3))
b10 <- readxl::read_xlsx("data-raw/Datastream_Switzerland_Assets.xlsx",sheet = "b10") %>%
  rename(DATE=Name,b10=!!names(.[2])) %>%
  mutate(DATE=as.yearmon(as.Date(DATE)),b10=as.numeric(b10))
libor <- readxl::read_xlsx("data-raw/Datastream_Switzerland_Assets.xlsx",sheet = "libor") %>%
  rename(DATE=Name,libor=!!names(.[2])) %>%
  mutate(DATE=as.yearmon(as.Date(DATE)),libor=as.numeric(libor))
infl <- readxl::read_xlsx("data-raw/Datastream_Switzerland_Assets.xlsx",sheet = "infl") %>%
  rename(DATE=Name,infl=!!names(.[2])) %>%
  mutate(DATE=as.yearmon(as.Date(DATE)),infl=as.numeric(infl))
reres <- readxl::read_xlsx("data-raw/Datastream_Switzerland_Assets.xlsx",sheet = "reres") %>%
  rename(DATE=Name,reres=!!names(.[2])) %>%
  mutate(DATE=as.yearmon(as.Date(DATE)),reres=as.numeric(reres))
recom <- readxl::read_xlsx("data-raw/Datastream_Switzerland_Assets.xlsx",sheet = "recom") %>%
  rename(DATE=Name,recom=!!names(.[2])) %>%
  mutate(DATE=as.yearmon(as.Date(DATE)),recom=as.numeric(recom))
##################################################################
## 2: Merge the relevant datapoints
# FINDATA<-merge(msci,fte,libor,b3,b10,infl,reres,recom)
# save(FINDATA,file = "FINDATA.RData")
FINDATA <- msci %>% left_join(fte,by="DATE") %>% left_join(libor,by="DATE") %>% left_join(b3,by="DATE") %>% left_join(b10,by="DATE") %>%
  left_join(infl,by="DATE") %>% left_join(reres,by="DATE") %>% left_join(recom,by="DATE") %>%
  mutate(DATE=as.Date(DATE)) %>%
  timetk::tk_xts()
save(FINDATA,file = "data-raw/FINDATA.RData")
usethis::use_data(FINDATA,overwrite=TRUE)
##################################################################
## 3: Select only relevant series
data("FINDATA")
# merge selected series, make price/index series from libor
D <- na.omit(merge(FINDATA$msci,FINDATA$b10,FINDATA$recom,cumprod(na.omit(1+FINDATA$libor/1200)),FINDATA$infl))
# calculate log-returns
R <- na.omit(Return.calculate(D,method="log"))

    # R<-na.omit(merge(Return.calculate(D,method = "log"),log(1+FINDATA$libor)))
    # V<-vars::VAR(R,1)
    # B<-Bcoef(V)[1:5,1:5]
    # c<-Bcoef(V)[1:5,6]
    # steady state
    # s<-solve(diag(5)-B)%*%c
##################################################################
## 4: Estimate VAR-Process
library(pensionfinanceLi)
# Adjust the real estate series
R$recom <- R$recom+0.03/4 # add 0.03/4 to account for real estate, evtl. change - see nsi.xls add 4.1%
### in MHAs original file: he uses the 3% correction but also privdes 3.5% and 4.1% "AssetReturnX.RData")
V <- tsDyn::lineVar(R , 1, model = "VAR")
B <- V$coefficients[1:5,2:6] # slope parameters
c <- V$coefficients[1:5,1] # intercepts

# calculate steady state
s <- solve(diag(5)-B)%*%c
print(exp(s*4)-1) # sanity check whether the process makes econmically sense
# calculate covariance matrix of residuals
covres <- var(V$residuals)/(nrow(R)-2-ncol(R))*(nrow(R)-2)
# save
save(V,file = "data-raw/VAR_Model.RData")
usethis::use_data(V,overwrite=TRUE)
##################################################################
## 5: Simulating Data
library(pensionfinanceLi)
data(V)
ret <- VARsim(V,simN = 10000)
# save
save(ret,file = "data-raw/VAR_return_sim.RData")
usethis::use_data(ret,overwrite=TRUE)
##################################################################
## 6: Creating real returns
library(pensionfinanceLi)
data(ret)
retr <- ret
retr <- apply(ret,3,function(x){x-x[,"infl"]})
dim(retr) <- dim(ret)
dimnames(retr) <- dimnames(ret)
# save
save(retr,file = "data-raw/VAR_returnreal_sim.RData")
usethis::use_data(retr,overwrite=TRUE)
##################################################################
## 6: Creating real returns
w2 <- setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl"))
SPFret <- list()
for (c_age in 20:70){
  SPFret[[ as.character(c_age) ]] <- list()
  for (ret_age in 60:70){
    if (c_age>=ret_age){next}
    ma <- retr[c_age:(ret_age-1),names(w2),,drop=FALSE]
    pf_ret <- apply(ma,3,function(x) (exp(x)-1)%*%w2) # discrete real returns times portfolio weights
    # necessary to keep matrix dimensions
    dim(pf_ret) <- c(length(c_age:(ret_age-1)),dim(ret)[3])
    # now limit max and min performance (equivalent to smoothed performance)
    # for all future periods and scenarios we limit
    ind25 <- which(pf_ret<quantile(pf_ret,.25))
    ind75 <- which(pf_ret>quantile(pf_ret,.75))
    # calculate actually paid returns as mean over all (smoothed)
    payed_ret <- pracma::ones(nrow(pf_ret),ncol(pf_ret))*mean(pf_ret)
    # the best/worst years get 2% more (less) than the overall mean
    payed_ret[ind25] <- mean(pf_ret)-.02
    payed_ret[ind75] <- mean(pf_ret)+.02
    rownames(payed_ret) <- as.character(c_age:(ret_age-1))
    SPFret[[ as.character(c_age) ]][[ as.character(ret_age)]] <- payed_ret
  }
}
save(SPFret,file = "data-raw/SPCF.RData")
usethis::use_data(SPFret,overwrite=TRUE)

