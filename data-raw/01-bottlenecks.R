# devtools::install_github("MarcoDVisser/aprof")

library(profvis)
library(lsts)

p <- profvis({
  block.smooth.periodogram(malleco)
})

p
