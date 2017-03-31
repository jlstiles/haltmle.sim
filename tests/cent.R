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

# load packages
library(modifySL)
library(haltmle.sim)
library(drtmle)

# parameters
ns <- c(100,500,1000,2000)
bigB <- 2000

# directories to save in 
saveDir <- "~/haltmle_sim/out"
scratchDir <- "~/haltmle_sim/scratch"

# # simulation parameters
parm <- expand.grid(seed=1:bigB,
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
    dat <- makeRandomData(n=parm$n[i], maxD = 8)
    save(dat, file=paste0(scratchDir,"/draw_n=",parm$n[i],"_seed=",parm$seed[i],".RData"))
  }
  print(paste0('initial datasets saved to: ~/hc/sim/inFile3 ... .RData'))
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
    load(paste0(saveDir,"/draw_n=",parm$n[i],"_seed=",parm$seed[i],".RData"))
    
    # load libraries
    library(SuperLearner)
    library(caret)
    
    algo <- c("SL.glm","SL.bayesglm", 
              "SL.earth", "SL.gammaLogGLM","SL.gammaIdentityGLM",
              "SL.stepAIC","SL.step","SL.svm.caret1",
              "SL.step.forward", "SL.step.interaction",
              "SL.gam", "SL.gbm.caret1", "SL.randomForest.caret1",
              "SL.svmLinear.caret1",
              "SL.nnet.caret1",
              "SL.rpart.caret1", "SL.mean","SL.hal")
        
    # fit super learner with all algorithms
    set.seed(parm$seed[i])
    X <- data.frame(dat$A, dat$W)
    fullSL <- SuperLearner(Y=dat$Y,X=X,family=gaussian(), SL.library=algo,
                                   verbose=TRUE,method=method.CC_LS)
    # modify SL to drop HAL
    dropSL <- modifySL(fit = fullSL, library = fullSL$libraryNames[-length(algo)])

    ## get inference
    X1 <- X0 <- X
    X1$trt <- 1; X0$trt <- 0
    X1$ageInt <- X1$age
    X1$femaleInt <- X1$female
    X1$raceInt <- X1$race
    X1$sofaInt <- X1$sofa
    X1$score <- X1$score
    X0[,grep("Int",names(X0))] <- 0
    
    out <- hc.tmle(Y=Y,X=X,X0=X0,X1=X1,fm=sl.mse.nosplit,onlySL=FALSE)

    save(out, file=paste0("~/hc/sim/out/outInf3_n=",parm$n[i],"_seed=",parm$seed[i],".RData"))
    }
  
  
}

# merge job ###########################
if (args[1] == 'merge') {
  n <- sl.mse.ns <- sl.mse.s <- sl.ll.ns <- sl.ll.s <- cand <- outliers <- NULL
  # get true value
  bigDat <- makeFCSData2(n=1e6)
  truth <- mean(bigDat$totalcost[bigDat$trt == 1]) - mean(bigDat$totalcost[bigDat$trt==0])
  allf <- list.files("~/hc/sim/out")
  theseF <- allF[grep("outInf3", allF)]
  for(i in 1:length(theseF)){
    out <- get(load(paste0("~/hc/sim/out/",theseF[i])))
    tmp <- strsplit(theseF[i], "=")
    n <- c(n, as.numeric(strsplit(tmp[[1]][2],"_seed")[[1]][1]))
    sl.mse.ns <- rbind(sl.mse.ns, out[[1]][[1]]$diff[1])
    sl.mse.s <- rbind(sl.mse.s, out[[2]][[1]]$diff[1])
    sl.ll.ns <- rbind(sl.ll.ns, out[[3]][[1]]$diff[1])
    sl.ll.s <- rbind(sl.ll.s, out[[4]][[1]]$diff[1])
    cand <- rbind(cand, Reduce(c, lapply(out[[1]][[3]], function(x){x[[3]][1]})))

    # count outliers in data set
    dat <- get(load(paste0("~/hc/sim/scratch/",gsub("outInf3","inFile",theseF[i]))))            
    tmp <- summary(dat$totalcost)
    outlier2 <- sum(dat$totalcost > tmp[3] + 2*(tmp[5]-tmp[2])) 
    outlier3 <- sum(dat$totalcost > tmp[3] + 3*(tmp[5]-tmp[2])) 
    outlier4 <- sum(dat$totalcost > tmp[3] + 4*(tmp[5]-tmp[2]))
    outliers <- rbind(outliers, c(outlier2, outlier3, outlier4))
  }
  algo <- c("SL.glm","SL.gammaLogGLM","SL.gammaIdentityGLM",        
            "SL.logOLS.smear","SL.manningGLM","SL.gengamma",
            "SL.weibull","SL.lognormalsurv",
            "SL.wangZhou","SL.coxph","SL.gilleskie",
            "SL.randomForest","SL.rpart","SL.gbm",
            "SL.rpart.caret1","SL.rf.caret1","SL.gbm.caret1",
            "SL.mean","SL.hal")
  
  out <- data.frame(n, sl.mse.ns,sl.mse.s,sl.ll.ns,sl.ll.s,cand,outliers)
  names(out) <- c("n",paste0("sl.",1:4), algo, paste0("out",2:4))
  out$truth <- truth

  save(out, file="~/hc/rslt3.RData")
}