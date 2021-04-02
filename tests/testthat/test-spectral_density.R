test_that("spectral.density works", {
  # Spectral Density AR(1)
  f <- spectral.density(ar = 0.5, lambda = malleco)
  expect_equal(round(f[1:3], 4), c(0.1554, 0.2130, 0.1843))
})
