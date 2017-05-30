#-------------------------------------------
# commands for debugging sim functions 
#-------------------------------------------
library(devtools)
library(roxygen2)
# building and installing
setwd("~/Dropbox/R/")
document("haltmle.sim")
build("haltmle.sim")
install("haltmle.sim")

# debugging makeRandomData
library(haltmle.sim)
#debug(makeRandomData)
object <- makeRandomData(n = 500, maxD = 6)
plot(object, ask = TRUE)
summary(object)
