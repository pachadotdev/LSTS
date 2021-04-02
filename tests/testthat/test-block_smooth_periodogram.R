test_that("block.smooth.periodogram works", {
  bsp <- block.smooth.periodogram(malleco)
  expect_equal(round(head(bsp$data$z, 3), 3), c(0.050, 0.002, 0.018))
})
