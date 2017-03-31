#-------------------------------------------
# commands for debugging sim functions 
#-------------------------------------------
library(devtools)
library(roxygen2)
# building and installing
setwd("~/Dropbox/Berkeley/")
document("haltmle.sim")
build("haltmle.sim")
install("haltmle.sim")


# debugging makeRandomData
library(haltmle.sim)
#debug(makeRandomData)
object <- makeRandomData(n = 500, maxD = 2)
plot(object)
summary(object)
