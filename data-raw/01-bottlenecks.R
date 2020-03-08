# devtools::install_github("MarcoDVisser/aprof")

library(profvis)
library(lsts)

p <- profvis({
  # block.smooth.periodogram(malleco)
  Box.Ljung.Test(malleco, lag = 5)
})

p
