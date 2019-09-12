data(V); data(ret); data(retr); load("fpCF_test.RData")

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
