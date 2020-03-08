test_that("ls_kalman works", {
  lsk <- ls_kalman(malleco, start(malleco))
  expect_equal(unique(lsk$delta), 1542564)
})
