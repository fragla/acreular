context("Testing utility functions")

test_that("datesToDuration returns the correct value", {
  onset <- as.Date("2010-01-01")
  assessment <- as.Date("2010-02-13")

  expect_equal(datesToDuration(onset, assessment), 43)

  expect_true(is.na(datesToDuration(NA, assessment)))
  expect_true(is.na(datesToDuration(onset, NA)))

  expect_error(datesToDuration("2010-01-01", assessment))

})
