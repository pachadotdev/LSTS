test_that("block_smooth_periodogram works", {
  bsp <- block_smooth_periodogram(malleco)
  expect_equal(round(diag(bsp),4), c(0.6432, 0.0000, 0.0000, 2.3321))
})
