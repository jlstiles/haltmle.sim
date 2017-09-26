#! /usr/bin/env Rscript 

# A script to merge files only

saveDir <- "~/haltmle.sim/out/"
scratchDir <- "~/haltmle.sim/scratch/"
library(modifySL, lib.loc = "~/R/x86_64-pc-linux-gnu-library/3.3")
library(haltmle.sim, lib.loc = "~/R/x86_64-pc-linux-gnu-library/3.3")
library(drtmle, lib.loc = "~/R/x86_64-pc-linux-gnu-library/3.3")

n <- sum <- slFull <- slDrop <- cand <- NULL
# get summary

# load all files in saveDir
allFiles <- list.files(saveDir)
# restrict to files from first batch
allBatch1Files <- allFiles[grep("drawOut_n=",allFiles)]
# rename for use with code below
allf <- allBatch1Files

# load and compile each result
for(i in 1:length(allf)){
	# print message every 500 files
	if(i %% 500 == 0){ cat(i,"\n") }

	out <- get(load(paste0(saveDir,allf[i])))
	tmp <- strsplit(allf[i], "=")
	n <- c(n, as.numeric(strsplit(tmp[[1]][2],"_seed")[[1]][1]))
	slFull <- rbind(slFull, out[[1]][[1]]$diff)
	slDrop <- rbind(slDrop, out[[2]][[1]]$diff)
	cand <- rbind(cand, Reduce(c, lapply(out[[1]][[3]], function(x){x$diff})))

	# count outliers in data set
	dat <- get(load(paste0(scratchDir,gsub("drawOut","draw",allf[i]))))          
	tmp <- summary(dat)
	sum <- rbind(sum, unlist(tmp))
}
algo <- c("SL.glm","SL.bayesglm", 
        "SL.earth",
        "SL.stepAIC","SL.step",
        "SL.step.forward", "SL.step.interaction",
        "SL.gam", "SL.gbm.caret1", "SL.randomForest.caret1",
        "SL.svmLinear.caret1",
        "SL.nnet.caret1",
        "SL.rpart.caret1", "SL.mean","SL.hal")

allOut <- data.frame(n, slFull, slDrop, cand, sum)
names(allOut) <- c("n",paste0("slFull.",c("est","cil","ciu")),
              paste0("slDrop.",c("est","cil","ciu")), 
              t(outer(algo,c(".est",".cil",".ciu"), FUN=paste0)),
              names(unlist(tmp)))
save(allOut, file=paste0(saveDir,"allOut.RData"))

# convert factors to numerics
allOut[,c(1:113,115)] <- apply(allOut[,c(1:113,115)], 2, as.numeric)

# oracle ci's
allOut$seOrc <- sqrt(allOut$varEffIC/allOut$n)
estNames <- colnames(allOut)[grep("\\.est",  colnames(allOut))]
allAlgos <- c("slFull","slDrop",algo)
for(i in 1:length(allAlgos)){
	eval(parse(text=paste0("allOut$",allAlgos[i],".cilOrc <- allOut[,paste0(allAlgos[i],'.est')] - 1.96*allOut$seOrc")))
	eval(parse(text=paste0("allOut$",allAlgos[i],".ciuOrc <- allOut[,paste0(allAlgos[i],'.est')] + 1.96*allOut$seOrc")))
}


# get rid of table values because they appear to not 
# have been added correctly
# allOut_reduced <- allOut[,!grepl("table",colnames(allOut))]

# save(allOut_reduced, file=paste0(saveDir,"allOut_reduced.RData"))