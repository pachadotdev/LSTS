test_that("LS.kalman works", {
  lsk <- LS.kalman(malleco, start(malleco))
  expect_equal(unique(lsk$delta), 1542564)
})
