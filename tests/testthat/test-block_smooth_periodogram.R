test_that("block_smooth_periodogram works", {
  bsp <- block_smooth_periodogram(malleco)
  expect_equal(round(head(bsp$data$z, 3), 3), c(0.050, 0.002, 0.018))
})
