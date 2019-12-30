context("Testing acreular RA scoring methods")

test_that("aprScore returns the correct AcrEularRA score", {
  test1 <- new_acrEularRA(apr="Normal")
  test2 <- new_acrEularRA(apr="Abnormal")
  test3 <- new_acrEularRA(apr="")
  test4 <- new_acrEularRA(apr=NA_character_)

  expect_equal(aprScore(test1), 0)
  expect_equal(aprScore(test2), 1)
  expect_equal(aprScore(test3), NA)
  expect_equal(aprScore(test4), NA)
})

test_that("aprClassification returns the correct value", {
  expect_equal(aprClassification(crp=10,esr=20, crp.uln=10, esr.uln=15), "Abnormal")
  expect_equal(aprClassification(crp=10,esr=10, crp.uln=10, esr.uln=15), "Normal")
  expect_equal(aprClassification(crp=NA,esr=10, crp.uln=10, esr.uln=15), "Normal")
  expect_equal(aprClassification(crp=11,esr=NA, crp.uln=10, esr.uln=15), "Abnormal")
  expect_equal(aprClassification(crp=NA,esr=NA, crp.uln=10, esr.uln=15), NA)

  expect_error(aprClassification(crp=10,esr=10, crp.uln=NA, esr.uln=NA))
})

test_that("durationScore returns the correct value", {
  test1 <- new_acrEularRA(duration=NA)
  test2 <- new_acrEularRA(duration=42)
  test3 <- new_acrEularRA(duration=43)
  ##test NA, <1, negative etc

  expect_true(is.na(durationScore(test1)))
  expect_true(durationScore(test2)==0)
  expect_true(durationScore(test3)==1)
})

test_that("jointScore returns the correct value", {
  expect_equal(jointScore(new_acrEularRA(ljc=1, sjc=0)), 0)
  expect_equal(jointScore(new_acrEularRA(ljc=2, sjc=0)), 1)
  expect_equal(jointScore(new_acrEularRA(ljc=10, sjc=0)), 1)
  expect_equal(jointScore(new_acrEularRA(ljc=0, sjc=1)), 2)
  expect_equal(jointScore(new_acrEularRA(ljc=0, sjc=3)), 2)
  expect_equal(jointScore(new_acrEularRA(ljc=2, sjc=3)), 2)
  expect_equal(jointScore(new_acrEularRA(ljc=1, sjc=9)), 3)
  expect_equal(jointScore(new_acrEularRA(ljc=7, sjc=4)), 5)

  expect_true(is.na(jointScore(new_acrEularRA(ljc=7, sjc=NA_real_))))
  expect_true(is.na(jointScore(new_acrEularRA(ljc=NA_real_, sjc=1))))

  expect_error(jointScore(new_acrEularRA(ljc=1.2, sjc=1)))
})

test_that("serologyScore returns the correct value", {
  test1 <- new_acrEularRA(serology="Negative")
  test2 <- new_acrEularRA(serology="Low")
  test3 <- new_acrEularRA(serology="High")
  test4 <- new_acrEularRA(serology="NA")
  test5 <- new_acrEularRA(serology="")

  expect_equal(serologyScore(test1), 0)
  expect_equal(serologyScore(test2), 2)
  expect_equal(serologyScore(test3), 3)
  expect_equal(serologyScore(test4), NA)
  expect_equal(serologyScore(test5), NA)
})

test_that("serologyClassification returns the correct value", {
  expect_equal(serologyClassification(ccp=10,rf=20, ccp.uln=10, rf.uln=20), "Negative")
  expect_equal(serologyClassification(ccp=10,rf=20, ccp.uln=10, rf.uln=20), "Negative")
  expect_equal(serologyClassification(ccp=11,rf=20, ccp.uln=10, rf.uln=20), "Low")
  expect_equal(serologyClassification(ccp=10,rf=61, ccp.uln=10, rf.uln=20), "High")
  expect_equal(serologyClassification(ccp=NA,rf=NA, ccp.uln=10, rf.uln=20), NA)

  expect_error(serologyClassification(ccp=10,rf=10, ccp.uln=NA, rf.uln=NA))
})

test_that("acrEularRAScore returns the correct value", {
  test1 <- new_acrEularRA(ljc=3,sjc=4,duration=60,apr="Normal", serology="Low")
  test2 <- new_acrEularRA(ljc=1,sjc=1,duration=62,apr="Abnormal", serology="Negative")
  expect_equal(acrEularRAScore(test1), 6)
  expect_equal(acrEularRAScore(test2), 4)
})

test_that("acrEularRAClassification returns the correct value", {
  test1 <- new_acrEularRA(ljc=3,sjc=4,duration=60,apr="Normal", serology="Low")
  test2 <- new_acrEularRA(ljc=1,sjc=1,duration=62,apr="Abnormal", serology="Negative")
  test3 <- new_acrEularRA(ljc=NA,sjc=NA,duration=NA,apr=NA, serology=NA)
  expect_equal(acrEularRAClassification(test1), "RA (ACR/EULAR 2010)")
  expect_equal(acrEularRAClassification(test2), "UA")

  expect_true(is.na(acrEularRAClassification(test3)))
})

test_that("acrEularRA helper function works correctly", {
  test1 <- acrEularRA(ljc=8, sjc=12, duration=43, apr="Normal", serology="High")
  test2 <- acrEularRA(ljc=8, sjc=12,
                     onset=as.Date("2010-01-01"), assessment=as.Date("2010-02-13"),
                     crp=5, esr=12, ccp=32, rf=71)

  expect_equal(test1, test2)

  expect_error(acrEularRA(duration=43, onset=as.Date("2010-01-01"), assessment=as.Date("2010-02-14")))
  expect_error(acrEularRA(apr="Normal", crp=5, esr=16))
  expect_error(acrEularRA(apr="Normal", crp=11, esr=12))
  expect_error(acrEularRA(serology="Negative", ccp=32, rf=20))
  expect_error(acrEularRA(serology="Negative", ccp=9, rf=21))

})

