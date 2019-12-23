context("Testing acreular RA scoring methods")

test1 <- new_acrEularRA(crp=10,esr=15)
test2 <- new_acrEularRA(crp=20,esr=20)
test3 <- new_acrEularRA(crp=11,esr=4)
test4 <- new_acrEularRA(crp=4,esr=16)

test_that("aprScore returns the correct AcrEularRA score", {
  expect_equal(aprScore(test1), 0)
  expect_equal(aprScore(test2), 1)
  expect_equal(aprScore(test3), 1)
  expect_equal(aprScore(test4), 1)
})

test_that("aprScore throws error for incorrect ULN parameters", {
  expect_error(aprScore(test1, ccp.uln=NA, esr.uln=NA))
  expect_error(aprScore(test2, ccp.uln="10", esr.uln="15"))
  expect_error(aprScore(test3, ccp.uln=numeric(), esr.uln=numeric()))
})

test_that("durationScore returns the correct value", {
  test1 <- new_acrEularRA(duration=NA_integer_)
  test2 <- new_acrEularRA(duration=42)
  test3 <- new_acrEularRA(duration=43)
  ##test NA, <1, negative etc

  expect_true(is.na(durationScore(test1)))
  expect_true(durationScore(test2)==0)
  expect_true(durationScore(test3)==1)
})

test_that("jointScore returns the correct value", {

  expect_true(jointScore(new_acrEularRA(ljc=1, sjc=0))==0)
  expect_true(jointScore(new_acrEularRA(ljc=2, sjc=0))==1)
  expect_true(jointScore(new_acrEularRA(ljc=10, sjc=0))==1)
  expect_true(jointScore(new_acrEularRA(ljc=0, sjc=1))==2)
  expect_true(jointScore(new_acrEularRA(ljc=0, sjc=3))==2)
  expect_true(jointScore(new_acrEularRA(ljc=2, sjc=3))==2)
  expect_true(jointScore(new_acrEularRA(ljc=1, sjc=9))==3)
  expect_true(jointScore(new_acrEularRA(ljc=7, sjc=4))==5)

  expect_true(is.na(jointScore(new_acrEularRA(ljc=7, sjc=NA_real_))))
  expect_true(is.na(jointScore(new_acrEularRA(ljc=NA_real_, sjc=1))))
})

test_that("serologyScore returns the correct value", {
  test1 <- new_acrEularRA(ccp=10,rf=20)
  test2 <- new_acrEularRA(ccp=11,rf=20)
  test3 <- new_acrEularRA(ccp=9,rf=21)
  test4 <- new_acrEularRA(ccp=31,rf=21)
  test5 <- new_acrEularRA(ccp=30,rf=61)

  expect_equal(serologyScore(test1), 0)
  expect_equal(serologyScore(test2), 2)
  expect_equal(serologyScore(test3), 2)
  expect_equal(serologyScore(test4), 3)
  expect_equal(serologyScore(test5), 3)
})

test_that("acrEularScore returns the correct value", {
  test1 <- new_acrEularRA(ljc=3,sjc=4,duration=60,crp=5, rf=21,ccp=11,esr=12)
  test2 <- new_acrEularRA(ljc=1,sjc=1,duration=62,crp=11, rf=20,ccp=5,esr=5)

  expect_equal(acrEularRAScore(test1), 6)
  expect_equal(acrEularRAScore(test2), 4)
})
# test_that("aprScore returns the correct AcrEularRA score", {
#   test1 <- acrEularRA(crp=10,esr=15)
#   test2 <- acrEularRA(crp=20,esr=20)
#   test3 <- acrEularRA(crp=11,esr=4)
#   test4 <- acrEularRA(crp=4,esr=16)
#
#   expect_equal(aprScore(test1), 0)
#   expect_equal(aprScore(test2), 1)
#   expect_equal(aprScore(test3), 1)
#   expect_equal(aprScore(test4), 1)
# })
#
# test_that("durationScore returns the correct value", {
#   test1 <- acrEularRA(duration=NA_integer_)
#   test2 <- acrEularRA(duration=42)
#   test3 <- acrEularRA(duration=43)
#   ##test NA, <1, negative etc
#
#   expect_true(is.na(durationScore(test1)))
#   expect_true(durationScore(test2)==0)
#   expect_true(durationScore(test3)==1)
# })
#
# test_that("jointScore returns the correct value", {
#
#   expect_true(jointScore(acrEularRA(ljc=1, sjc=0))==0)
#   expect_true(jointScore(acrEularRA(ljc=2, sjc=0))==1)
#   expect_true(jointScore(acrEularRA(ljc=10, sjc=0))==1)
#   expect_true(jointScore(acrEularRA(ljc=0, sjc=1))==2)
#   expect_true(jointScore(acrEularRA(ljc=0, sjc=3))==2)
#   expect_true(jointScore(acrEularRA(ljc=2, sjc=3))==2)
#   expect_true(jointScore(acrEularRA(ljc=1, sjc=9))==3)
#   expect_true(jointScore(acrEularRA(ljc=7, sjc=4))==5)
#
#   expect_true(is.na(jointScore(acrEularRA(ljc=7, sjc=NA_real_))))
#   expect_true(is.na(jointScore(acrEularRA(ljc=NA_real_, sjc=1))))
# })
#
# test_that("serologyScore returns the correct value", {
#   test1 <- acrEularRA(ccp=10,rf=20)
#   test2 <- acrEularRA(ccp=11,rf=20)
#   test3 <- acrEularRA(ccp=9,rf=21)
#   test4 <- acrEularRA(ccp=31,rf=21)
#   test5 <- acrEularRA(ccp=30,rf=61)
#
#   expect_equal(serologyScore(test1), 0)
#   expect_equal(serologyScore(test2), 2)
#   expect_equal(serologyScore(test3), 2)
#   expect_equal(serologyScore(test4), 3)
#   expect_equal(serologyScore(test5), 3)
# })
#
# test_that("acrEularScore returns the correct value", {
#   test1 <- acrEularRA(ljc=3,sjc=4,duration=60,crp=5, rf=21,ccp=11,esr=12)
#   test2 <- acrEularRA(ljc=1,sjc=1,duration=62,crp=11, rf=20,ccp=5,esr=5)
#
#   expect_equal(acrEularScore(test1), 6)
#   expect_equal(acrEularScore(test2), 4)
# })
#
