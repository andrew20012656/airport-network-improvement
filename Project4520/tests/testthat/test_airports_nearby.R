test_that("Check if there are only 4 airports within 75 miles from Ithaca", {
  # Ithaca's coordinate
  lonlat = matrix(c(-76.45872, 42.49136), nrow = 1, ncol = 2)
  load("/Users/andrewliu/Desktop/2022_Spring/STSCI_4520/BTRY_4520/Project4520/data/airport_data.RData")
  #load("/Users/shirley/Desktop/statcomp2022/BTRY_4520/Project4520/data")
  result <- airports_nearby(lonlat, airport_data, 75)
  # Expect 4 airports within 75 miles of Ithaca's coordinate
  expect_equal(
    nrow(result),
    4
  )
})

