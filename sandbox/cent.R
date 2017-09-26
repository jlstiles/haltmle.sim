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
  library(haltmle.sim, lib.loc = "/home/dbenkese/R/x86_64-unknown-linux-gnu-library/3.2")
  library(cvma, lib.loc = "/home/dbenkese/R/x86_64-unknown-linux-gnu-library/3.2")
  library(hal9001, lib.loc = "/home/dbenkese/R/x86_64-unknown-linux-gnu-library/3.2")
# })

# for hc_tmle
# source("~/hc/sim/healthcosts.R")

# full parm
# ns <- c(200, 1000, 5000)
ns <- c(200)
min_R2 <- c(0.01, seq(0.101, 0.901, by = 0.1))
max_R2 <- c(seq(0.1,0.9,0.1),0.99)
mat_R2 <- cbind(min_R2, max_R2)
bigB <- 2500

# # simulation parameters
parm <- expand.grid(seed=1:bigB,
                    n=ns,
                    range_R2 = split(mat_R2, row(mat_R2)))

# directories to save in 
saveDir <- "~/haltmle.sim/out/"
scratchDir <- "~/haltmle.sim/scratch/"

# get the list size #########
if (args[1] == 'listsize') {
  cat(nrow(parm))
}

# execute prepare job ##################
if (args[1] == 'prepare') {
  for(i in 1:nrow(parm)){
    set.seed(parm$seed[i])
    dat <- haltmle.sim:::makeRandomData(n=parm$n[i], maxD = 8,
                                        minR2 = parm$range_R2[[i]][1],
                                        maxR2 = parm$range_R2[[i]][2])
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
    
    algo <- c("SL.hal9001",
              "SL.glm",
              "SL.bayesglm", 
              "SL.earth",
              "SL.step.interaction",
              "SL.gam", 
              "SL.dbarts.mod",
              "SL.gbm.caretMod",
              "SL.rf.caretMod",
              "SL.rpart.caretMod", 
              "SL.mean",
              "SL.kernelKnn")
        
    # fit super learner with all algorithms
    set.seed(parm$seed[i])
    dat$W <- data.frame(dat$W)
    colnames(dat$W) <- paste0("W",1:ncol(dat$W))
    system.time(
    out <- get_all_ates(Y = dat$Y$Y, A = dat$A$A, W = dat$W, 
                        V = 10, learners = algo, remove_learner = NULL)
    )
    save(out, file=paste0(saveDir,"out_n=",parm$n[i],"_seed=",parm$seed[i],
                          "_r2="parm$range_R2[[i]][1],".RData"))
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


