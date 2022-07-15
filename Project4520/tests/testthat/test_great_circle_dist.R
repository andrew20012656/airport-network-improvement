test_that("Great circle dist is working", {
  lonlat1 <- matrix(c(-76, 42), nrow = 1, ncol = 2)
  lonlat2 <- matrix(c(-74, 42), nrow = 1, ncol = 2)
  # Calculation from online source gives value 102.96 miles
  # Expecting the error to be within 5 miles
  result <- (abs(distfun(lonlat1, lonlat2) - 102.96) < 1)[1,1]
  expect_equal(
    result,
    TRUE
  )
})

