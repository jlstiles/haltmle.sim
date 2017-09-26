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
# #debug(makeRandomData)
object <- makeRandomData(n = 500, maxD = 1)
# undebug(haltmle.sim:::plot.makeRandomData)
plot(object, ask = FALSE, addMain = FALSE, nrow = 2, ncol = 2)
summary(object)


# plots for WNAR talk

pdf("~/Dropbox/Berkeley/Talks/WNAR2017/simExample1.pdf",height=5,width=6)
set.seed(12345)
object <- makeRandomData(n = 500, maxD = 1)
plot(object, ask = FALSE, addMain = FALSE, nrow = 2, ncol = 2)
dev.off()

set.seed(124856125)
object <- makeRandomData(n = 500, maxD = 1)
plot(object, ask = FALSE, addMain = FALSE, nrow = 2, ncol = 2)

pdf("~/Dropbox/Berkeley/Talks/WNAR2017/simExample2.pdf",height=5,width=6)
set.seed(1284236)
object <- makeRandomData(n = 500, maxD = 1)
plot(object, ask = FALSE, addMain = FALSE, nrow = 2, ncol = 2)
dev.off()

#----------------------------------
# make Univariate plots for slides
#----------------------------------

for(i in 1:50){
	set.seed(i)
	object <- makeRandomData(n = 500, maxD = 1)
	pdf(paste0("~/Dropbox/Berkeley/Talks/WNAR2017/simMovie_Bivariate",i,".pdf"),height=5,width=8)
	plot(object, ask = FALSE, addMain = FALSE, nrow = 2, ncol = 3)
	dev.off()
}
#----------------------------------
# make Bivariate plots for slides
#----------------------------------
layOutNum <- c(1,3,4,2,3,4,5,6,7,5,6,7)
nBiv <- 0
ct <- 0
while(nBiv != 10){
	ct <- ct + 1
	set.seed(ct)
	object <- makeRandomData(n = 500, maxD = 2, funcQ0.uni = c("linUni","polyUni"),
	                         funcG0.uni = c("linUni","polyUni"), funcQ0.biv = c("linBiv"),
	                         funcG0.biv = c("linBiv"))
	if(ncol(object$W) == 2){
		nBiv <- nBiv + 1
		# pdf(paste0("~/Dropbox/Berkeley/Talks/WNAR2017/tmp",nBiv,".pdf"),height=5,width=8)
		# plot(object, ask = FALSE, addMain = FALSE, layOutNum = layOutNum, ncol = 3, nrow = 4)
		# dev.off()
	}
}

#--------------------------------------
# make plots of error and coverage
#--------------------------------------

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

# source in plotting functions until they're in the package
source("~/Dropbox/R/haltmle.sim/R/summarizeBias.R")
# load result data.frame
load(paste0(saveDir,"allOut.RData"))

# colors
slCol <- rgb(0,0,1,0.75)
halCol <- rgb(1,0,0,0.75)
otherCol <- rgb(0,0,0,0.25)

#---------------------------
# by dimension
#---------------------------
for(pct in c(TRUE,FALSE)){
pdf(paste0("~/Dropbox/Berkeley/Talks/WNAR2017/biasByDimension",
           ifelse(pct, "Pct", "Z"),
           "_mean.pdf"),width = 6, height = 4.5)
par(mgp=c(1.5,0.5,0), mar = c(3.1,3.1,2.1,0.5),oma=c(0,0,1,0))
plotBias(result = allOut, 
         estNames = c("SL.glm.est","SL.gbm.caret1.est","SL.randomForest.caret1.est",
                      "SL.earth.est","SL.svmLinear.caret1.est","SL.gam.est","SL.nnet.caret1.est",
                      "SL.step.interaction.est","SL.hal.est","slFull.est"),
         estLabels = c("SL","GLM","GBM","RF","MARS","SVM","GAM","NNET","STEPGLM","HAL","SL"),
         strat = "D",
         pointOptions = list(pch = 10:1,col=c(rep(otherCol,8),slCol,halCol),
                             lwd = c(rep(1,8),2,2)),
         xlab = "Covariate dimension",
         legendOptions = list(x = ifelse(pct,"topleft","topright"),lty=rep(NA,10),
                              legend = c("GLM","GBM","RF","MARS","SVM","GAM","NNET","STEPGLM","HAL","SL")), 
         main = "",
         pctTruth = pct,
         summary = list(fn="mean",options=list(trim = 0,na.rm=TRUE)),
         mgp=c(1.5,0.5,0), mar = c(3.1,3.1,2.1,0.5),oma=c(0,0,1,0))
mtext(outer=TRUE, text = "Mean absolute error",line=-0.9,cex=1.5,at=0.55)
dev.off()


pdf(paste0("~/Dropbox/Berkeley/Talks/WNAR2017/biasByDimension",
           ifelse(pct, "Pct", "Z"),
           "_99quantile.pdf"),width = 6, height = 4.5)
par(mgp=c(1.5,0.5,0), mar = c(3.1,3.1,2.1,0.5),oma=c(0,0,1,0))
plotBias(result = allOut, 
         estNames = c("SL.glm.est","SL.gbm.caret1.est","SL.randomForest.caret1.est",
                      "SL.earth.est","SL.svmLinear.caret1.est","SL.gam.est","SL.nnet.caret1.est",
                      "SL.step.interaction.est","SL.hal.est","slFull.est"),
         estLabels = c("SL","GLM","GBM","RF","MARS","SVM","GAM","NNET","STEPGLM","HAL","SL"),
         strat = "D",
	     pctTruth = pct, 
         pointOptions = list(pch = 10:1,col=c(rep(otherCol,8),slCol,halCol),
                             lwd = c(rep(1,8),2,2)),
         xlab = "Covariate dimension",
         legendOptions = list(x = ifelse(pct,"topleft","topright"),lty=rep(NA,10),
                              legend = c("GLM","GBM","RF","MARS","SVM","GAM","NNET","STEPGLM","HAL","SL")), 
         main = "",
         summary = list(fn="quantile",options = list(probs = c(0.99), na.rm = TRUE)),
         mgp=c(1.5,0.5,0), mar = c(3.1,3.1,2.1,0.5),oma=c(0,0,1,0))
mtext(outer=TRUE, text = "0.99 quantile absolute error",line=-0.9,cex=1.5,at=0.55)
dev.off()


pdf(paste0("~/Dropbox/Berkeley/Talks/WNAR2017/biasByDimension",
           ifelse(pct, "Pct", "Z"),
           "_median.pdf"),width = 6, height = 4.5)
par(mgp=c(1.5,0.5,0), mar = c(3.1,3.1,2.1,0.5),oma=c(0,0,1,0))
plotBias(result = allOut, 
         estNames = c("SL.glm.est","SL.gbm.caret1.est","SL.randomForest.caret1.est",
                      "SL.earth.est","SL.svmLinear.caret1.est","SL.gam.est","SL.nnet.caret1.est",
                      "SL.step.interaction.est","SL.hal.est","slFull.est"),
         estLabels = c("SL","GLM","GBM","RF","MARS","SVM","GAM","NNET","STEPGLM","HAL","SL"),
         strat = "D",
         pctTruth = pct, 
         pointOptions = list(pch = 10:1,col=c(rep(otherCol,8),slCol,halCol),
                             lwd = c(rep(1,8),2,2)),
         xlab = "Covariate dimension",
         legendOptions = list(x = ifelse(pct,"topleft","topright"),lty=rep(NA,10),
                              legend = c("GLM","GBM","RF","MARS","SVM","GAM","NNET","STEPGLM","HAL","SL")), 
         main = "",
         summary = list(fn="quantile",options = list(probs = c(0.5), na.rm = TRUE)),
         mgp=c(1.5,0.5,0), mar = c(3.1,3.1,2.1,0.5),oma=c(0,0,1,0))
mtext(outer=TRUE, text = "Median absolute error",line=-0.9,cex=1.5,at=0.55)
dev.off()
}
#---------------------------------
# by sample size
#---------------------------------
for(pct in c(TRUE,FALSE)){
pdf(paste0("~/Dropbox/Berkeley/Talks/WNAR2017/biasByN",
           ifelse(pct,"Pct","Z"),
           "_mean.pdf"),width = 6, height = 4.5)
par(mgp=c(1.5,0.5,0), mar = c(3.1,3.1,2.1,0.5),oma=c(0,0,1,0))
plotBias(result = allOut, 
         estNames = c("SL.glm.est","SL.gbm.caret1.est","SL.randomForest.caret1.est",
                      "SL.earth.est","SL.svmLinear.caret1.est","SL.gam.est","SL.nnet.caret1.est",
                      "SL.step.interaction.est","SL.hal.est","slFull.est"),
         estLabels = c("SL","GLM","GBM","RF","MARS","SVM","GAM","NNET","STEPGLM","HAL","SL"),
         strat = "n",
         pctTruth = pct, 
         pointOptions = list(pch = 10:1,col=c(rep(otherCol,8),slCol,halCol),
                             lwd = c(rep(1,8),2,2)),
         xlab = "Sample size",
         legendOptions = list(x = ifelse(pct,"topright","topleft"),lty=rep(NA,10),
                              legend = c("GLM","GBM","RF","MARS","SVM","GAM","NNET","STEPGLM","HAL","SL")), 
         main = "",
         summary = list(fn="mean",options = list(trim = 0, na.rm=TRUE)),
         mgp=c(1.5,0.5,0), mar = c(3.1,3.1,2.1,0.5),oma=c(0,0,1,0))
mtext(outer=TRUE, text = "Mean absolute error",line=-0.9,cex=1.5,at=0.55)
dev.off()


pdf(paste0("~/Dropbox/Berkeley/Talks/WNAR2017/biasByN",
           ifelse(pct,"Pct","Z"),
           "_99quantile.pdf"),width = 6, height = 4.5)
par(mgp=c(1.5,0.5,0), mar = c(3.1,3.1,2.1,0.5),oma=c(0,0,1,0))
plotBias(result = allOut, 
         estNames = c("SL.glm.est","SL.gbm.caret1.est","SL.randomForest.caret1.est",
                      "SL.earth.est","SL.svmLinear.caret1.est","SL.gam.est","SL.nnet.caret1.est",
                      "SL.step.interaction.est","SL.hal.est","slFull.est"),
         estLabels = c("SL","GLM","GBM","RF","MARS","SVM","GAM","NNET","STEPGLM","HAL","SL"),
         strat = "n",
         pctTruth = pct, 
         pointOptions = list(pch = 10:1,col=c(rep(otherCol,8),slCol,halCol),
                             lwd = c(rep(1,8),2,2)),
         xlab = "Sample size",
         legendOptions = list(x = ifelse(pct,"topright","topleft"),lty=rep(NA,10),
                              legend = c("GLM","GBM","RF","MARS","SVM","GAM","NNET","STEPGLM","HAL","SL")), 
         main = "",
         summary = list(fn="quantile",options = list(probs = c(0.99),na.rm=TRUE)),
         mgp=c(1.5,0.5,0), mar = c(3.1,3.1,2.1,0.5),oma=c(0,0,1,0))
mtext(outer=TRUE, text = "0.99 quantile absolute error",line=-0.9,cex=1.5,at=0.55)
dev.off()


pdf(paste0("~/Dropbox/Berkeley/Talks/WNAR2017/biasByN",
           ifelse(pct,"Pct","Z"),
           "_median.pdf"),width = 6, height = 4.5)
par(mgp=c(1.5,0.5,0), mar = c(3.1,3.1,2.1,0.5),oma=c(0,0,1,0))
plotBias(result = allOut, 
         estNames = c("SL.glm.est","SL.gbm.caret1.est","SL.randomForest.caret1.est",
                      "SL.earth.est","SL.svmLinear.caret1.est","SL.gam.est","SL.nnet.caret1.est",
                      "SL.step.interaction.est","SL.hal.est","slFull.est"),
         estLabels = c("SL","GLM","GBM","RF","MARS","SVM","GAM","NNET","STEPGLM","HAL","SL"),
         strat = "n",          
         pctTruth = pct, 
         pointOptions = list(pch = 10:1,col=c(rep(otherCol,8),slCol,halCol),
                             lwd = c(rep(1,8),2,2)),
         xlab = "Sample size",
         legendOptions = list(x = ifelse(pct,"topright","topleft"),lty=rep(NA,10),
                              legend = c("GLM","GBM","RF","MARS","SVM","GAM","NNET","STEPGLM","HAL","SL")), 
         main = "",
         summary = list(fn="quantile",options = list(probs = c(0.5),na.rm=TRUE)),
         mgp=c(1.5,0.5,0), mar = c(3.1,3.1,2.1,0.5),oma=c(0,0,1,0))
mtext(outer=TRUE, text = "Median absolute error",line=-0.9,cex=1.5,at=0.55)
dev.off()
}

#-------------------------------
# coverage plots
#-------------------------------
# add oracle CIs to results frame
algo <- c("SL.glm","SL.bayesglm", 
        "SL.earth",
        "SL.stepAIC","SL.step",
        "SL.step.forward", "SL.step.interaction",
        "SL.gam", "SL.gbm.caret1", "SL.randomForest.caret1",
        "SL.svmLinear.caret1",
        "SL.nnet.caret1",
        "SL.rpart.caret1", "SL.mean","SL.hal")
# oracle ci's
allOut$seOrc <- sqrt(allOut$varEffIC/allOut$n)
estNames <- colnames(allOut)[grep("\\.est",  colnames(allOut))]
allAlgos <- c("slFull","slDrop",algo)
for(i in 1:length(allAlgos)){
	eval(parse(text=paste0("allOut$",allAlgos[i],".cilOrc <- allOut[,paste0(allAlgos[i],'.est')] - 1.96*allOut$seOrc")))
	eval(parse(text=paste0("allOut$",allAlgos[i],".ciuOrc <- allOut[,paste0(allAlgos[i],'.est')] + 1.96*allOut$seOrc")))
}

#-------------------
# by dimension
#-------------------
pdf("~/Dropbox/Berkeley/Talks/WNAR2017/coverageByDimension.pdf",width = 6, height = 4.5)
par(mgp=c(1.5,0.5,0), mar = c(3.1,3.1,2.1,0.5),oma=c(0,0,0,0))
plotCoverage(result = allOut, 
             cilNames = c("SL.glm.cilOrc","SL.gbm.caret1.cilOrc","SL.randomForest.caret1.cilOrc",
                      "SL.earth.cilOrc","SL.svmLinear.caret1.cilOrc","SL.gam.cilOrc","SL.nnet.caret1.cilOrc",
                      "SL.step.interaction.cilOrc","SL.hal.cilOrc","slFull.cilOrc"),
             ciuNames = c("SL.glm.ciuOrc","SL.gbm.caret1.ciuOrc","SL.randomForest.caret1.ciuOrc",
                      "SL.earth.ciuOrc","SL.svmLinear.caret1.ciuOrc","SL.gam.ciuOrc","SL.nnet.caret1.ciuOrc",
                      "SL.step.interaction.ciuOrc","SL.hal.ciuOrc","slFull.ciuOrc"),
         	 estLabels = c("GLM","GBM","RF","MARS","SVM","GAM","NNET","STEPGLM","HAL","SL"),
         	 strat = "D",ylim = c(0.7,1),
         	 pointOptions = list(pch = 10:1,col=c(rep(otherCol,8),slCol,halCol),
                             lwd = c(rep(1,8),2,2)),
         	 xlab = "Covariate dimension",
         	 mgp=c(1.5,0.5,0), mar = c(3.1,3.1,2.1,0.5),oma=c(0,0,0,0),
         	 legendOptions = list(x = "bottomright", legend =c("GLM","GBM","RF","MARS","SVM","GAM","NNET","STEPGLM","HAL","SL"),
         	                      lty = rep(NA,10)),
         	 main = "")
dev.off()


pdf("~/Dropbox/Berkeley/Talks/WNAR2017/coverageByN.pdf",width = 6, height = 4.5)
par(mgp=c(1.5,0.5,0), mar = c(3.1,3.1,2.1,0.5),oma=c(0,0,0,0))
plotCoverage(result = allOut, 
             cilNames = c("SL.glm.cilOrc","SL.gbm.caret1.cilOrc","SL.randomForest.caret1.cilOrc",
                      "SL.earth.cilOrc","SL.svmLinear.caret1.cilOrc","SL.gam.cilOrc","SL.nnet.caret1.cilOrc",
                      "SL.step.interaction.cilOrc","SL.hal.cilOrc","slFull.cilOrc"),
             ciuNames = c("SL.glm.ciuOrc","SL.gbm.caret1.ciuOrc","SL.randomForest.caret1.ciuOrc",
                      "SL.earth.ciuOrc","SL.svmLinear.caret1.ciuOrc","SL.gam.ciuOrc","SL.nnet.caret1.ciuOrc",
                      "SL.step.interaction.ciuOrc","SL.hal.ciuOrc","slFull.ciuOrc"),
         	 estLabels = c("GLM","GBM","RF","MARS","SVM","GAM","NNET","STEPGLM","HAL","SL"),
         	 strat = "n",ylim = c(0.7,1),
         	 pointOptions = list(pch = 10:1,col=c(rep(otherCol,8),slCol,halCol),
                             lwd = c(rep(1,8),2,2)),
         	 xlab = "Sample size",
         	 mgp=c(1.5,0.5,0), mar = c(3.1,3.1,2.1,0.5),oma=c(0,0,0,0),
         	 legendOptions = list(x = "bottomright", legend =c("GLM","GBM","RF","MARS","SVM","GAM","NNET","STEPGLM","HAL","SL"),
         	                      lty = rep(NA,10)),
         	 main = "")
dev.off()

#---------------------
# comparing to GLM
#---------------------
# standard GLM
pdf("~/Dropbox/Berkeley/Talks/WNAR2017/glmComp_smallN.pdf",width = 7, height = 4.5)
plotDiff(result = allOut[allOut$n==100,], estNames = c("SL.glm.est","slFull.est"),
                 ylab = "P(|Z-GLM| - |Z-SL| < x)")
dev.off()

pdf("~/Dropbox/Berkeley/Talks/WNAR2017/glmComp_largeN.pdf",width = 7, height = 4.5)
plotDiff(result = allOut[allOut$n>=1000,], estNames = c("SL.glm.est","slFull.est"),
                 ylab = "P(|Z-GLM| - |Z-SL| < x)")
dev.off()

# standard GLM
pdf("~/Dropbox/Berkeley/Talks/WNAR2017/glmComp_smallD.pdf",width = 7, height = 4.5)
plotDiff(result = allOut[allOut$D <= 2,], estNames = c("SL.glm.est","slFull.est"),
                 ylab = "P(|Z-GLM| - |Z-SL| < x)")
dev.off()

pdf("~/Dropbox/Berkeley/Talks/WNAR2017/glmComp_largeD.pdf",width = 7, height = 4.5)
plotDiff(result = allOut[allOut$D >= 6,], estNames = c("SL.glm.est","slFull.est"),
                 ylab = "P(|Z-GLM| - |Z-SL| < x)")
dev.off()

# stepwise GLM
pdf("~/Dropbox/Berkeley/Talks/WNAR2017/glmStepComp_smallN.pdf",width = 7, height = 4.5)
plotDiff(result = allOut[allOut$n==100,], estNames = c("SL.step.interaction.est","slFull.est"),
                 ylab = "P(|Z-STEPGLM| - |Z-SL| < x)")
dev.off()

pdf("~/Dropbox/Berkeley/Talks/WNAR2017/glmStepComp_largeN.pdf",width = 7, height = 4.5)
plotDiff(result = allOut[allOut$n>=1000,], estNames = c("SL.step.interaction.est","slFull.est"),
                 ylab = "P(|Z-STEPGLM| - |Z-SL| < x)")
dev.off()

# standard GLM
pdf("~/Dropbox/Berkeley/Talks/WNAR2017/glmStepComp_smallD.pdf",width = 7, height = 4.5)
plotDiff(result = allOut[allOut$D <= 2,], estNames = c("SL.step.interaction.est","slFull.est"),
                 ylab = "P(|Z-STEPGLM| - |Z-SL| < x)")
dev.off()

pdf("~/Dropbox/Berkeley/Talks/WNAR2017/glmStepComp_largeD.pdf",width = 7, height = 4.5)
plotDiff(result = allOut[allOut$D >= 6,], estNames = c("SL.step.interaction.est","slFull.est"),
                 ylab = "P(|Z-STEPGLM| - |Z-SL| < x)")
dev.off()


#----------------------------------------------
# best and worst sims compared to competitors
#----------------------------------------------
allOut$absZ.hal <- abs(sqrt(allOut$n)*(allOut$SL.hal.est - allOut$truth)/sqrt(allOut$varEffIC))
allOut$absZ.glm <- abs(sqrt(allOut$n)*(allOut$SL.glm.est - allOut$truth)/sqrt(allOut$varEffIC))

allOut$intTermsG0 <- with(allOut, bivTermsG0 + triTermsG0)
allOut$intTermsQ0 <- with(allOut, bivTermsQ0 + triTermsQ0)

# get best data sets
bestData <- findBestSims(result = allOut,
                        estName = "SL.hal.est",pctTruth = FALSE, 
                        compEstNames = paste0(algo[1:14],".est"),
                        nSims = 2200)
worstData <- findWorstSims(result = allOut,
                        estName = "SL.hal.est",pctTruth = FALSE, 
                        compEstNames = paste0(algo[c(1:10,13:14)],".est"),
                        nSims = 1000)
# Performance
#    Z_hal average (not losing by much in worst case)
# Covariates
#    dimension
#    number binary
#    number categorical
#    number continuous
#    with correlation > 0.4
# Propensity
#    interactions
#    min g_0(W)
#    noisy covariates
# Outcome regression
#    interactions
#    R^2 
#    noisy covariates
# Error distribution
#    errors correlated with W

char <- c("n","absZ.hal","D","numBinW",
          "numCatW","numContW","nCorr4",
          "intTermsG0","minObsG0","numNoisyG0",
          "intTermsQ0","r2","numNoisyQ0",
          "corSqErrW")

bestInd <- as.numeric(row.names(bestData$bestResults))
worstInd <- as.numeric(row.names(worstData$worstResults))
#summary(allOut[-bestInd,char])

# means for best descriptive simulations
bestMeans <- colMeans(bestData$bestResults[,char],
                      na.rm = TRUE)
worstMeans <- colMeans(worstData$worstResults[,char],
                      na.rm = TRUE)
# means for rest of simulations
restMeans <- colMeans(allOut[-c(bestInd,worstInd),char],na.rm=TRUE)

# do a t-test 
pval <- rep(NA, length(char))
for(i in 1:length(char)){
    pval[i] <- t.test(
        x = bestData$bestResults[,char[i]],
        y = worstData$worstResults[,char[i]],
        var.equal = FALSE, paired = FALSE
    )$p.value
}

tmp <- data.frame(best = bestMeans, worst = worstMeans, rest = restMeans, pval = round(pval,3))
row.names(tmp) <- c("Sample size","Z_HAL","Covariate dim.","Binary W",
                    "Categorical W","Continuous W","Covariates with correlation $>$ 0.4",
                    "Interaction terms $g_0$","$mbox{min}_w g_0(w)$", "Covariates not assoc. with PS",
                    "Interaction terms $bar{Q}_0$","$R^2$ for OR","Covariates not assoc. with OR",
                    "Avg. correlation of W with error")
library(xtable)
xtable(tmp[c(1,2,4)])

#----------------------------------------------
# best and worst sims by absolute 
#----------------------------------------------
allOut$absZ.hal <- abs(sqrt(allOut$n)*(allOut$SL.hal.est - allOut$truth)/sqrt(allOut$varEffIC))
allOut$absZ.glm <- abs(sqrt(allOut$n)*(allOut$SL.glm.est - allOut$truth)/sqrt(allOut$varEffIC))

# get best data sets
bestData <- findBestSims(result = allOut,compEst = FALSE, 
                        estName = "SL.hal.est",pctTruth = FALSE, 
                        compEstNames = paste0(algo[1:14],".est"),
                        nSims = 2200)
worstData <- findWorstSims(result = allOut,compEst = FALSE,threshWorst = 3,
                        estName = "SL.hal.est",pctTruth = FALSE, 
                        compEstNames = paste0(algo[c(1:10,13:14)],".est"),
                        nSims = 1000)


char <- c("n","absZ.hal","D","numBinW",
          "numCatW","numContW","nCorr4",
          "intTermsG0","minObsG0","numNoisyG0",
          "intTermsQ0","r2","numNoisyQ0",
          "corSqErrW")


bestInd <- as.numeric(row.names(bestData$bestResults))
worstInd <- as.numeric(row.names(worstData$worstResults))
#summary(allOut[-bestInd,char])

# means for best descriptive simulations
bestMeans <- colMeans(bestData$bestResults[,char],
                      na.rm = TRUE)
worstMeans <- colMeans(worstData$worstResults[,char],
                      na.rm = TRUE)
# means for rest of simulations
restMeans <- colMeans(allOut[-c(bestInd,worstInd),char],na.rm=TRUE)

# do a t-test 
pval <- rep(NA, length(char))
for(i in 1:length(char)){
    pval[i] <- t.test(
        x = bestData$bestResults[,char[i]],
        y = worstData$worstResults[,char[i]],
        var.equal = FALSE, paired = FALSE
    )$p.value
}

tmp <- data.frame(best = bestMeans, worst = worstMeans, rest = restMeans, pval = round(pval,3))
row.names(tmp) <- c("Sample size","Z_HAL","Covariate dim.","Binary W",
                    "Categorical W","Continuous W","Covariates with correlation $>$ 0.4",
                    "Interaction terms $g_0$","$mbox{min}_w g_0(w)$", "Covariates not assoc. with PS",
                    "Interaction terms $bar{Q}_0$","$R^2$ for OR","Covariates not assoc. with OR",
                    "Avg. correlation of W with error")
xtable(tmp[c(1,2,4)])
