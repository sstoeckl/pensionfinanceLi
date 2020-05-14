# data(V); data(ret); data(retr);
prefix <- ""; # prefix <- "D:/Onedrive-UniLi-ROOT/Packages/pensionfinanceLi/tests/testthat/"
load(paste0(prefix,"fpCF_test.RData")); load(paste0(prefix,"spCF_test.RData"));
load(paste0(prefix,"tpCFwork_test.RData")); load(paste0(prefix,"tpCFwork_test.RData"));load(paste0(prefix,"tpCFret_test.RData"));
load(paste0(prefix,"totalCF_test.RData")); load(paste0(prefix,"util_test.RData"))
free_cf_before_tax <- c(81000.00,81810.00,82628.10,83454.38,84288.92,85131.81,85983.13,86842.96,87711.39,88588.51,89474.39,90369.14,91272.83,
                        92185.56,93107.41,94038.49,94978.87,95928.66,96887.95,97856.82,98835.39,99823.75,100821.98)
MortalityTables::mortalityTables.load("Austria_Annuities")

test_that("return generation works", {
  expect_equal(dim(VARsim(V,simN=10)), c(122*4/4,V$k,10))
  expect_equal(dim(ret), c(122*4/4,V$k,10000))
  expect_equal(dim(retr), c(122*4/4,V$k,10000))
})

test_that("first pillar CF works", {
  expect_equal(dim(fpCF(ret_age=65,c_age=42,li=100000,lg=0.01,s1=c(15,80000),ret=ret[,,1:10])), c(58,10))
  expect_equal(fpCF(ret_age=65,c_age=42,li=100000,lg=0.01,s1=c(15,80000),ret=ret[,,1:10]), fpCF_test)
  expect_equal(fpCF(ret_age=65,c_age=64,li=0,lg=0,s1=c(0,0),ret=ret[,,1:10]), fpCF_test2)
})

test_that("second pillar CF works", {
  expect_equal(length(spCF(ret_age=65,nu2=.5,c_age=42,c2=.12,li=100000,lg=0.01,
                           w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
                           ret=ret[,,1:10],retr=retr[,,1:10],s2=300000,rho2=0.05)), 3)
  expect_equal(dim(spCF(ret_age=65,nu2=.5,c_age=42,c2=.12,li=100000,lg=0.01,
                        w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
                        ret=ret[,,1:10],retr=retr[,,1:10],s2=300000,rho2=0.05)$pension), c(58,10))
  expect_equal(length(spCF(ret_age=65,nu2=.5,c_age=42,c2=.12,li=100000,lg=0.01,
                           w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
                           ret=ret[,,1:10],retr=retr[,,1:10],s2=300000,rho2=0.05)$lumpsum), 10)
  expect_equal(length(spCF(ret_age=65,nu2=.5,c_age=42,c2=.12,li=100000,lg=0.01,
                           w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
                           ret=ret[,,1:10],retr=retr[,,1:10],s2=300000,rho2=0.05)$wealth), 10)
  expect_equal(spCF(ret_age=65,nu2=.5,c_age=42,c2=.12,li=100000,lg=0.01,
                    w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
                    ret=ret[,,1:10],retr=retr[,,1:10],s2=300000,rho2=0.05), sp_test)
  expect_equal(spCF(ret_age=65,nu2=0.01,c_age=64,c2=0.01,li=0,lg=0.01,
                    w2=setNames(c(1,0,0,0,0),c("msci","b10","recom","libor","infl")),
                    ret=ret[,,1:10],retr=retr[,,1:10],s2=0,rho2=0.0000001), sp_test2)
})

test_that("third pillar CF during savings phase works", {
  expect_equal(length(tpCFwork(ret_age=65,c_age=42,w3=setNames(c(.25,.25,.25,.25,0),c("msci","b10","recom","libor","infl")),free_cf_before_tax=free_cf_before_tax,retr=retr[,,1:10],s3=300000,w0=300000,psi=0.015,c=0.6)), 2)
  expect_equal(dim(tpCFwork(ret_age=65,c_age=42,w3=setNames(c(.25,.25,.25,.25,0),c("msci","b10","recom","libor","infl")),free_cf_before_tax=free_cf_before_tax,retr=retr[,,1:10],s3=300000,w0=300000,psi=0.015,c=0.6)$cons),
               dim(tpCFwork(ret_age=65,c_age=42,w3=setNames(c(.25,.25,.25,.25,0),c("msci","b10","recom","libor","infl")),free_cf_before_tax=free_cf_before_tax,retr=retr[,,1:10],s3=300000,w0=300000,psi=0.015,c=0.6)$wealth))
  expect_equal(tpCFwork(ret_age=65,c_age=42,w3=setNames(c(.25,.25,.25,.25,0),c("msci","b10","recom","libor","infl")),free_cf_before_tax=free_cf_before_tax,retr=retr[,,1:10],s3=300000,w0=300000,psi=0.015,c=0.6), tpwork_test, tolerance=1e-7)
  expect_equal(tpCFwork(ret_age=65,c_age=42,w3=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),free_cf_before_tax=free_cf_before_tax,retr=retr[,,1:10],s3=0,w0=0,psi=0.015,c=1), tpwork_test2, tolerance=1e-7)
})

test_that("Taxes during savings phase", {
  expect_equal(length(taxCFwork(income=c(100000,100000),liquid_wealth=c(10000,300000),illiquid_Wealth = c(300000,300000))$from_cf),
               length(taxCFwork(income=c(100000,100000),liquid_wealth=c(10000,300000),illiquid_Wealth = c(300000,300000))$from_liquid_wealth))
  expect_equal(taxCFwork(income=c(100000,100000),liquid_wealth=c(10000,300000),illiquid_Wealth = c(300000,300000))$from_cf, c(7712.5,7712.5))
  expect_equal(taxCFwork(income=c(100000,100000),liquid_wealth=c(10000,300000),illiquid_Wealth = c(300000,300000))$from_liquid_wealth, c(1727.5,3467.5))
})

test_that("Taxes for lumpsumpayments", {
  expect_equal(length(taxCFlumpsum(lumpsum=c(100000,500000,1000000),ret_age = 65,gender=0)), 3)
  expect_equal(taxCFlumpsum(lumpsum=1000000,ret_age = 65,gender=c(0,1)), setNames(c(939393.3,942854.1),c("0","1")))
})

test_that("Taxes for Pension Payments", {
  expect_equal(length(taxCFret(fpcf=c(15000,15000),totalcf=c(30000,30000),wealth=c(10000,100000))$from_cf),
               length(taxCFret(fpcf=c(15000,15000),totalcf=c(30000,30000),wealth=c(10000,100000))$from_wealth))
  expect_equal(taxCFret(fpcf=c(15000,15000),totalcf=c(100000,100000),wealth=c(10000,100000))$from_cf, c(6400, 6400))
  expect_equal(taxCFret(fpcf=c(15000,15000),totalcf=c(100000,100000),wealth=c(10000,100000))$from_wealth, c(50, 500))
})

test_that("third pillar CF during retirement phase works", {
  expect_equal(length(tpCFret(ret_age=65,c_age=42,w3=setNames(c(.25,.25,.25,.25,0),c("msci","b10","recom","libor","infl")),
                              alpha=0.96,wealth_at_ret_age=100000,retr=retr[,,1:10],psi=0.015)), 2)
  expect_equal(dim(tpCFret(ret_age=65,c_age=42,w3=setNames(c(.25,.25,.25,.25,0),c("msci","b10","recom","libor","infl")),
                           alpha=0.96,wealth_at_ret_age=100000,retr=retr[,,1:10],psi=0.015)$cons),
               dim(tpCFret(ret_age=65,c_age=42,w3=setNames(c(.25,.25,.25,.25,0),c("msci","b10","recom","libor","infl")),
                           alpha=0.96,wealth_at_ret_age=100000,retr=retr[,,1:10],psi=0.015)$wealth))
  expect_equal(tpCFret(ret_age=65,c_age=42,w3=setNames(c(.25,.25,.25,.25,0),c("msci","b10","recom","libor","infl")),
                       alpha=0.96,wealth_at_ret_age=100000,retr=retr[,,1:10],psi=0.015), tpret_test, tolerance=1e-7)
  expect_equal(tpCFret(ret_age=65,c_age=42,w3=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
                       alpha=0.96,wealth_at_ret_age=c(rep(-10000,5),rep(10000,5)),retr=retr[,,1:10],psi=0), tpret_test2, tolerance=1e-7)
})

test_that("total CF works", {
  expect_equal(length(totalCF(ret_age=65,c_age=42,
                              w3=setNames(c(.25,.25,.25,.25,0),c("msci","b10","recom","libor","infl")),
                              c=0.6,c2=.12,nu2=.5,nu3=0.01,gender=0,
                              w0=300000,li=100000,lg=0.01,c1=0.07,s1=c(15,80000),s2=300000,s3=300000,
                              w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
                              rho2=0.05,rho3=0.04,ret=ret[,,1:10],retr=retr[,,1:10],psi=0.015,alpha=0.96)), 3)
  expect_equal(dim(totalCF(ret_age=65,c_age=42,
                           w3=setNames(c(.25,.25,.25,.25,0),c("msci","b10","recom","libor","infl")),
                           c=0.6,c2=.12,nu2=.5,nu3=0.01,gender=0,
                           w0=300000,li=100000,lg=0.01,c1=0.07,s1=c(15,80000),s2=300000,s3=300000,
                           w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
                           rho2=0.05,rho3=0.04,ret=ret[,,1:10],retr=retr[,,1:10],psi=0.015,alpha=0.96)$cons),
               dim(rbind(totalCF(ret_age=65,c_age=42,
                           w3=setNames(c(.25,.25,.25,.25,0),c("msci","b10","recom","libor","infl")),
                           c=0.6,c2=.12,nu2=.5,nu3=0.01,gender=0,
                           w0=300000,li=100000,lg=0.01,c1=0.07,s1=c(15,80000),s2=300000,s3=300000,
                           w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
                           rho2=0.05,rho3=0.04,ret=ret[,,1:10],retr=retr[,,1:10],psi=0.015,alpha=0.96)$wealth_before_ret,
                   totalCF(ret_age=65,c_age=42,
                           w3=setNames(c(.25,.25,.25,.25,0),c("msci","b10","recom","libor","infl")),
                           c=0.6,c2=.12,nu2=.5,nu3=0.01,gender=0,
                           w0=300000,li=100000,lg=0.01,c1=0.07,s1=c(15,80000),s2=300000,s3=300000,
                           w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
                           rho2=0.05,rho3=0.04,ret=ret[,,1:10],retr=retr[,,1:10],psi=0.015,alpha=0.96)$wealth_after_ret)))
  expect_equal(totalCF(ret_age=65,c_age=42,
                      w3=setNames(c(.25,.25,.25,.25,0),c("msci","b10","recom","libor","infl")),
                      c=0.6,c2=.12,nu2=.5,nu3=0.01,gender=0,
                      w0=300000,li=100000,lg=0.01,c1=0.07,s1=c(15,80000),s2=300000,s3=300000,
                      w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
                      rho2=0.05,rho3=0.04,ret=ret[,,1:10],retr=retr[,,1:10],psi=0.015,alpha=0.96), totalcf_test, tolerance=1e-7)
  expect_equal(totalCF(ret_age=65,c_age=64,
                      w3=setNames(c(1,0,0,0,0),c("msci","b10","recom","libor","infl")),
                      c=1,c2=0,nu2=0,nu3=0,gender=0,
                      w0=0,li=100,lg=0,c1=0,s1=c(0,0),s2=0,s3=0,
                      w2=setNames(c(0,0,0,1,0),c("msci","b10","recom","libor","infl")),
                      rho2=0.01,rho3=0.01,ret=ret[,,1:10],retr=retr[,,1:10],psi=0.015,alpha=0.96), totalcf_test2, tolerance=1e-7)
})

test_that("utility works", {
  expect_equal(length(util(ret_age=65,c_age=42,
                          tw3=setNames(c(.25,.25,.25),c("msci","b10","recom")),
                          c=0.6,c2=.12,nu2=.5,nu3=0.01,ra=4,delta=0.02,alpha=0.96,beta=0.75,gender=0,
                          gender_mortalityTable=MortalityTables::baseTable(AVOe2005R.male),
                          w0=300000,CF=NULL,li=100000,lg=0.01,c1=0.07,s1=c(15,80000),s2=300000,s3=300000,
                          w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
                          rho2=0.05,rho3=0.04,ret=ret[,,1:10],retr=retr[,,1:10],psi=0.015)), 1)
  expect_equal(util(ret_age=65,c_age=42,
                    tw3=setNames(c(.25,.25,.25),c("msci","b10","recom")),
                    c=0.6,c2=.12,nu2=.5,nu3=0.01,ra=4,delta=0.02,alpha=0.96,beta=0.75,gender=0,
                    gender_mortalityTable=MortalityTables::baseTable(AVOe2005R.male),
                    w0=300000,CF=NULL,li=100000,lg=0.01,c1=0.07,s1=c(15,80000),s2=300000,s3=300000,
                    w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),
                    rho2=0.05,rho3=0.04,ret=ret[,,1:10],retr=retr[,,1:10],psi=0.015), util_test, tolerance=1e-7)
 expect_equal(util(ret_age=65,c_age=64,
                     tw3=setNames(c(1,0,0),c("msci","b10","recom")),
                     c=1,c2=0,nu2=0,nu3=0,ra=0,delta=0,alpha=1,beta=0,gender=0,
                     gender_mortalityTable=MortalityTables::baseTable(AVOe2005R.male),
                     w0=0,CF=NULL,li=0,lg=0,c1=0,s1=c(0,0),s2=0,s3=0,
                     w2=setNames(c(0,0,0,1,0),c("msci","b10","recom","libor","infl")),
                     rho2=0.0001,rho3=0.0001,ret=ret[,,1:10],retr=retr[,,1:10],psi=0), util_test2, tolerance=1e-7)
})

