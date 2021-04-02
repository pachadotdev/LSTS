test_that("Box.Ljung.Test works", {
  blt <- Box.Ljung.Test(malleco, lag = 5)
  expect_equal(blt$data$y, rep(0, 5))
})
