#! /usr/bin/env Rscript 
# This file was used to submit the simulation files to 
# a slurm-based Unix system. Using the sce.sh shell script
# one can submit each simulation in sequence. First, data files are
# created for each simulation. Those data files are then analyzed 
# in the 'run' execution. Then the results are collated in the 'merge'
# execution. 

# get environment variables
MYSCRATCH <- Sys.getenv('MYSCRATCH')
RESULTDIR <- Sys.getenv('RESULTDIR')
STEPSIZE <- as.numeric(Sys.getenv('STEPSIZE'))
TASKID <- as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))

# set defaults if nothing comes from environment variables
MYSCRATCH[is.na(MYSCRATCH)] <- '.'
RESULTDIR[is.na(RESULTDIR)] <- '.'
STEPSIZE[is.na(STEPSIZE)] <- 1
TASKID[is.na(TASKID)] <- 0

# get command lines arguments
args <- commandArgs(trailingOnly = TRUE)
if(length(args) < 1){
  stop("Not enough arguments. Please use args 'listsize', 'prepare', 'run <itemsize>' or 'merge'")
}

# install haltmle.sim 
# withr::with_libpaths("~/R packages",{
  # install_github("benkeser/haltmle.sim")
  # install_github("benkeser/modifySL")
  # install_github("benkeser/drtmle")
  # # load packages
  library(modifySL)
  library(haltmle.sim)
  library(drtmle)
# })

# for hc_tmle
source("~/hc/sim/healthcosts.R")

# parameters
ns <- c(4000)
bigB <- 1:10000

# directories to save in 
saveDir <- "~/haltmle.sim/out/"
scratchDir <- "~/haltmle.sim/scratch/"

# # simulation parameters
parm <- expand.grid(seed=bigB,
                    n=ns)

# get re-dos
# allF <- list.files("~/haltmle_sim/out")
# theseF <- allF[grep("outInf3", allF)]
# ind <- rep(NA, nrow(parm))
# for(i in 1:nrow(parm)){
#   ind[i] <- paste0("outInf3_n=",parm$n[i],"_seed=",parm$seed[i],".RData") %in% theseF
# }

# parm <- parm[!ind,]

# save(parm, file = "~/hc/sim/scratch/parm_remainSims.RData")
# load("~/hc/sim/scratch/parm_remainSims.RData")

# get the list size #########
if (args[1] == 'listsize') {
  cat(nrow(parm))
}

# execute prepare job ##################
if (args[1] == 'prepare') {
  for(i in 1:nrow(parm)){
    set.seed(parm$seed[i])
    dat <- haltmle.sim:::makeRandomData(n=parm$n[i], maxD = 8)
    save(dat, file=paste0(scratchDir,"draw_n=",parm$n[i],"_seed=",parm$seed[i],".RData"))
  }
  print(paste0('initial datasets saved to: ', scratchDir))
}

# execute parallel job #################################################
if (args[1] == 'run') {
  if (length(args) < 2) {
    stop("Not enough arguments. 'run' needs a second argument 'id'")
  }
  id <- as.numeric(args[2])
  print(paste(Sys.time(), "arrid:" , id, "TASKID:",
              TASKID, "STEPSIZE:", STEPSIZE))
  for (i in (id+TASKID):(id+TASKID+STEPSIZE-1)) {
    print(paste(Sys.time(), "i:" , i))

    # load parameters

    print(parm[i,])
    
    # load data
    load(paste0(scratchDir,"draw_n=",parm$n[i],"_seed=",parm$seed[i],".RData"))
    
    # load libraries
    library(SuperLearner)
    library(caret)
    
    algo <- c("SL.glm","SL.bayesglm", 
              "SL.earth",
              "SL.stepAIC","SL.step",
              "SL.step.forward", "SL.step.interaction",
              "SL.gam", "SL.gbm.caret1", "SL.rf.caret1",
              "SL.svmLinear.caret1",
              "SL.nnet.caret1",
              "SL.rpart.caret1", "SL.mean","SL.hal")
        
    # fit super learner with all algorithms
    set.seed(parm$seed[i])
    X <- data.frame(dat$A, dat$W)
    fullSL <- SuperLearner(Y=dat$Y$Y,X=X,family=gaussian(), SL.library=algo,
                                   verbose=TRUE,method="method.NNLS")
    # modify SL to drop HAL
    dropSL <- modifySL::modifySL(fit = fullSL, Y = dat$Y$Y, 
                                 newLibrary = fullSL$libraryNames[-(length(algo))])

    ## get inference
    X1 <- X0 <- X
    X1$A <- 1; X0$A <- 0
    
    outFull <- hc.tmle(Y=dat$Y$Y,X=X,X0=X0,X1=X1,fm=fullSL,onlySL=FALSE,trt="A")
    outDrop <- hc.tmle(Y=dat$Y$Y,X=X,X0=X0,X1=X1,fm=dropSL,onlySL=TRUE,trt="A")
    out <- list(outFull, outDrop)
    save(out, file=paste0(saveDir,"drawOut_V2_n=",parm$n[i],"_seed=",parm$seed[i],".RData"))
    }
}

# merge job ###########################
if (args[1] == 'merge') {
  # n <- sum <- slFull <- slDrop <- cand <- NULL
  # # get summary

  # # load all files in saveDir
  # allf <- list.files(saveDir)
  # fullf <- allf[grep("Full",allf)]
  # for(i in 1:length(allf)){
  #   if(i %% 100 == 0){ cat(i,"\n") }
  #   out <- get(load(paste0(saveDir,allf[i])))
  #   tmp <- strsplit(allf[i], "=")
  #   n <- c(n, as.numeric(strsplit(tmp[[1]][2],"_seed")[[1]][1]))
  #   slFull <- rbind(slFull, out[[1]][[1]]$diff)
  #   slDrop <- rbind(slDrop, out[[2]][[1]]$diff)
  #   cand <- rbind(cand, Reduce(c, lapply(out[[1]][[3]], function(x){x$diff})))

  #   # count outliers in data set
  #   dat <- get(load(paste0(scratchDir,gsub("drawOut","draw",allf[i]))))          
  #   tmp <- haltmle.sim:::summary.makeRandomData(dat)
  #   sum <- rbind(sum, unlist(tmp))
  # }
  # algo <- c("SL.glm","SL.bayesglm", 
  #           "SL.earth",
  #           "SL.stepAIC","SL.step",
  #           "SL.step.forward", "SL.step.interaction",
  #           "SL.gam", "SL.gbm.caret1", "SL.randomForest.caret1",
  #           "SL.svmLinear.caret1",
  #           "SL.nnet.caret1",
  #           "SL.rpart.caret1", "SL.mean","SL.hal")
  
  # allOut <- data.frame(n, slFull, slDrop, cand, sum)
  # names(allOut) <- c("n",paste0("slFull.",c("est","cil","ciu")),
  #                 paste0("slDrop.",c("est","cil","ciu")), 
  #                 outer(algo,c(".est",".cil",".ciu"), FUN=paste0),
  #                 names(unlist(tmp)))
  # save(allOut, file=paste0(saveDir,"allOut.RData"))
}


