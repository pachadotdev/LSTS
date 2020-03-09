test_that("box_ljung_test works", {
  blt <- box_ljung_test(malleco, lag = 5)
  expect_equal(blt$data$y, rep(0, 5))
})
