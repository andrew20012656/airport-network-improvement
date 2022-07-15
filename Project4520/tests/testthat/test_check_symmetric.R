test_that("Check symmetric case", {
  direct_from <- list(ITH = c("DTW"), DTW = c("ITH"))
  result <- check_symmetric(direct_from)
  expect_equal(
    result$symm,
    TRUE
  )
})

test_that("Check asymmetric case", {
  direct_from <- list(ITH = c("JFK"), DTW = c("ITH"), JFK = c("DTW"))
  result <- check_symmetric(direct_from)
  expect_equal(
    result$symm,
    FALSE
  )
})
