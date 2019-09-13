data(V); data(ret); data(retr); load("fpCF_test.RData"); load("spCF_test.RData")

test_that("return generation works", {
  expect_equal(dim(VARsim(V,simN=10)), c(122*4/4,V$k,10))
  expect_equal(dim(ret), c(122*4/4,V$k,10000))
  expect_equal(dim(retr), c(122*4/4,V$k,10000))
})

test_that("first pillar CF works", {
  expect_equal(dim(fpCF(ret_age=65,c_age=42,li=100000,lg=0.01,s1=c(15,80000),ret[,,1:10])), c(58,10))
  expect_equal(fpCF(ret_age=65,c_age=42,li=100000,lg=0.01,s1=c(15,80000),ret[,,1:10]), fpCF_test)
  expect_equal(fpCF(ret_age=65,c_age=64,li=0,lg=0,s1=c(0,0),ret[,,1:10]), fpCF_test2)
})

test_that("first pillar CF works", {
  expect_equal(length(spCF(ret_age=65,nu2=.5,c_age=42,c2=.12,li=100000,lg=0.01,w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),ret=ret[,,1:10],retr=retr[,,1:10],s2=300000,rho2=0.05)), 2)
  expect_equal(dim(spCF(ret_age=65,nu2=.5,c_age=42,c2=.12,li=100000,lg=0.01,w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),ret=ret[,,1:10],retr=retr[,,1:10],s2=300000,rho2=0.05)$pension), c(58,10))
  expect_equal(length(spCF(ret_age=65,nu2=.5,c_age=42,c2=.12,li=100000,lg=0.01,w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),ret=ret[,,1:10],retr=retr[,,1:10],s2=300000,rho2=0.05)$lumpsum), 10)
  expect_equal(spCF(ret_age=65,nu2=.5,c_age=42,c2=.12,li=100000,lg=0.01,w2=setNames(c(.30,.30,.30,.10,0),c("msci","b10","recom","libor","infl")),ret=ret[,,1:10],retr=retr[,,1:10],s2=300000,rho2=0.05), spCF_test)
  expect_equal(spCF(ret_age=65,nu2=0.01,c_age=64,c2=0.01,li=0,lg=0.01,w2=setNames(c(1,0,0,0,0),c("msci","b10","recom","libor","infl")),ret=ret[,,1:10],retr=retr[,,1:10],s2=0,rho2=0.0000001), spCF_test2)
})
