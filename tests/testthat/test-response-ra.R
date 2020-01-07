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
