context("Testing RA response functions")

test_that("eularResponse returns the correct value", {
  expect_equal(eularResponse(3.64, 2.44), "Good")
  expect_equal(eularResponse(3.64, 2.64), "Moderate")
  expect_equal(eularResponse(2.64, 2.55), "No response")
  expect_equal(eularResponse(5.50, 3.3), "Moderate")
  expect_equal(eularResponse(4.00, 3.3), "Moderate")
  expect_equal(eularResponse(4.71, 4.60), "No response")
  expect_equal(eularResponse(6.57, 5.21), "Moderate")
  expect_equal(eularResponse(6.00, 5.35), "No response")
  expect_equal(eularResponse(6.23, 6.05), "No response")

  expect_error(eularResponse(6.23, "6.oo"))
  expect_error(eularResponse(6.23, c(2.54, 3.46)))
})

test_that("acrResponse returns the correct value", {
  acr1 <- acrRA(tjc=6, sjc=6, ptGA=17, ptPain=11, phGA=60, haq=3.0, apr=24)
  acr2 <- acrRA(tjc=1, sjc=1, ptGA=1, ptPain=1, phGA=5, haq=0.85, apr=5)
  acr3 <- acrRA(tjc=10, sjc=5, ptGA=50, ptPain=30, phGA=60, haq=1.0, apr=10)
  acr4 <- acrRA(tjc=5, sjc=2, ptGA=20, ptPain=14, phGA=30, haq=0.85, apr=5)
  acr5 <- acrRA(tjc=20, sjc=15, ptGA=10, ptPain=30, phGA=60, haq=NA, apr=10)
  acr6 <- acrRA(tjc=15, sjc=5, ptGA=8, ptPain=20, phGA=30, haq=1.0, apr=8)

  expect_equal(acrResponse(acr1, acr2), "ACR70")
  expect_equal(acrResponse(acr3, acr4), "ACR50")
  expect_equal(acrResponse(acr5, acr6), "ACR20")
  expect_equal(acrResponse(acr2, acr3), "No response")
})
