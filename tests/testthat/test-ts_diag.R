test_that("ts_diag works", {
  tsd <- ts_diag(malleco)
  expect_equal(tsd$mfrow, c(3,1))
})
