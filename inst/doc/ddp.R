## -----------------------------------------------------------------------------
library(ddp)
data("simulasi")
kalori(simulasi, output = "energi")
#all output
kalori(simulasi)

## -----------------------------------------------------------------------------
skorpph(simulasi, wilayah = "Banten")
skorpph(simulasi, wilayah = "Maluku")

## -----------------------------------------------------------------------------
set.seed(1)
dat <- matrix(sample(1:7,10*5, replace = TRUE), 10,5)
valid(dat)

