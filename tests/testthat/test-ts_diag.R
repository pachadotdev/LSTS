test_that("ts_diag works", {
  tsd <- ts_diag(malleco)
  expect_equal(tsd$data$y, rep(0, 10))
})
