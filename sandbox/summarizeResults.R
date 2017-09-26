#-------------------------------------------------------
# executable commands to make summary plots and tables
#-------------------------------------------------------

# are you working locally or on unix
unix <- FALSE

if(unix){
	saveDir <- "~/haltmle.sim/out/"
	library(modifySL, lib.loc = "~/R/x86_64-pc-linux-gnu-library/3.3")
	library(haltmle.sim, lib.loc = "~/R/x86_64-pc-linux-gnu-library/3.3")
	library(drtmle, lib.loc = "~/R/x86_64-pc-linux-gnu-library/3.3")
}else{
	saveDir <- "~/Dropbox/R/haltmle.sim/results/"
}

# load result data.frame
load(paste0(saveDir,"allOut.RData"))

#=================================================================================
# PLOTTING BIAS
#=================================================================================

#----------------------------
# plot of bias by dimension
#----------------------------
plotBias(result = allOut, 
         estNames = c("SL.hal.est","slFull.est","slDrop.est","SL.glm.est"),
         estLabels = c("HAL","SL + HAL","SL","GLM"),
         strat = "D",
         xlab = "Covariate dimension")

#-----------------------------
# plot of bias by sample size
#-----------------------------
plotBias(result = allOut, 
         estNames = c("SL.hal.est","slFull.est","slDrop.est","SL.glm.est"),
         estLabels = c("HAL","SL + HAL","SL","GLM"),
         strat = "n",
         xlab = "Sample size",
         legendOptions = list(x = "topright", legend = c("HAL","SL + HAL","SL","GLM")))

#=================================================================================
# PLOTTING COVERAGE
#=================================================================================

#-------------------------------
# plot coverage by dimension
#-------------------------------
# estimated CIs
plotCoverage(result = allOut, 
             cilNames = c("SL.hal.cil","slFull.cil","slDrop.cil","SL.glm.cil"),
             ciuNames = c("SL.hal.ciu","slFull.ciu","slDrop.ciu","SL.glm.ciu"),
         	 estLabels = c("HAL","SL + HAL","SL","GLM"),
         	 strat = "D",
         	 xlab = "Covariate dimension",
         	 legendOptions = list(x = "topright", legend = c("HAL","SL + HAL","SL","GLM"),
         	                      inset = -0.05, xpd=TRUE),
         	 main = "Estimated SE")
# oracle CIs
plotCoverage(result = allOut, 
             cilNames = c("SL.hal.cilOrc","slFull.cilOrc","slDrop.cilOrc","SL.glm.cilOrc"),
             ciuNames = c("SL.hal.ciuOrc","slFull.ciuOrc","slDrop.ciuOrc","SL.glm.ciuOrc"),
         	 estLabels = c("HAL","SL + HAL","SL","Step/Int GLM","GLM"),
         	 strat = "D",
         	 xlab = "Covariate dimension",
         	 legendOptions = list(x = "bottomright", legend = c("HAL","SL + HAL","SL","GLM"),
         	                      inset = -0.05, xpd=TRUE),
         	 main = "Asymptotic SE")
#-------------------------------
# plot coverage by sample size
#-------------------------------
# estimated CIs
plotCoverage(result = allOut, 
             cilNames = c("SL.hal.cil","slFull.cil","slDrop.cil","SL.glm.cil"),
             ciuNames = c("SL.hal.ciu","slFull.ciu","slDrop.ciu","SL.glm.ciu"),
         	 estLabels = c("HAL","SL + HAL","SL","GLM"),
         	 strat = "n",
         	 xlab = "Sample size",
         	 legendOptions = list(x = "topright", legend = c("HAL","SL + HAL","SL","GLM"),
         	                      inset = -0.05, xpd=TRUE),
         	 main = "Estimated SE")
# oracle CIs
plotCoverage(result = allOut, 
             cilNames = c("SL.hal.cilOrc","slFull.cilOrc","slDrop.cilOrc","SL.glm.cilOrc"),
             ciuNames = c("SL.hal.ciuOrc","slFull.ciuOrc","slDrop.ciuOrc","SL.glm.ciuOrc"),
         	 estLabels = c("HAL","SL + HAL","SL","GLM"),
         	 strat = "n",
         	 xlab = "Sample size",
         	 legendOptions = list(x = "bottomright", legend = c("HAL","SL + HAL","SL","GLM"),
         	                      inset = -0.05, xpd=TRUE),
         	 main = "Asymptotic SE")

# by sample size only dimension 6-8
# estimated CIs
plotCoverage(result = allOut, 
             cilNames = c("SL.hal.cil","slFull.cil","slDrop.cil","SL.glm.cil"),
             ciuNames = c("SL.hal.ciu","slFull.ciu","slDrop.ciu","SL.glm.ciu"),
         	 estLabels = c("HAL","SL + HAL","SL","GLM"),
         	 strat = "n",
         	 xlab = "Sample size",
         	 legendOptions = list(x = "topright", legend = c("HAL","SL + HAL","SL","GLM"),
         	                      inset = -0.05, xpd=TRUE),
         	 main = "Estimated SE")

# oracle CIs
plotCoverage(result = allOut[allOut$D > 5,], 
             cilNames = c("SL.hal.cilOrc","slFull.cilOrc","slDrop.cilOrc","SL.glm.cilOrc"),
             ciuNames = c("SL.hal.ciuOrc","slFull.ciuOrc","slDrop.ciuOrc","SL.glm.ciuOrc"),
         	 estLabels = c("HAL","SL + HAL","SL","GLM"),
         	 strat = "n",
         	 xlab = "Sample size",
         	 legendOptions = list(x = "bottomright", legend = c("HAL","SL + HAL","SL","GLM"),
         	                      inset = -0.05, xpd=TRUE),
         	 main = "Asymptotic SE (dim. > 5)")



