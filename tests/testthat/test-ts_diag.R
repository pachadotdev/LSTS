test_that("ts.diag works", {
  tsd <- ts.diag(malleco)
  expect_equal(tsd$data$y, rep(0, 10))
})
