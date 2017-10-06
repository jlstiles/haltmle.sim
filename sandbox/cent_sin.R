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

# # load packages
library(arm)
library(plyr)
library(gam, lib.loc = "/home/dbenkese/R/x86_64-unknown-linux-gnu-library/3.2")
library(caret)
library(haltmle.sim, lib.loc = "/home/dbenkese/R/x86_64-unknown-linux-gnu-library/3.2")
library(Rsolnp, lib.loc = "/home/dbenkese/R/x86_64-unknown-linux-gnu-library/3.2")
library(future, lib.loc = "/home/dbenkese/R/x86_64-unknown-linux-gnu-library/3.2")
library(cvma, lib.loc = "/home/dbenkese/R/x86_64-unknown-linux-gnu-library/3.2")
library(hal9001, lib.loc = "/home/dbenkese/R/x86_64-unknown-linux-gnu-library/3.2")
library(drtmle, lib.loc = "/home/dbenkese/R/x86_64-unknown-linux-gnu-library/3.2")
library(SuperLearner)
# full parm
ns <- c(200, 1000, 5000)
# ns <- c(200)
bigB <- 1000
four_period <- c(0.5, 1, 5, 10)
two_period <- c(0.5, 10)

# # simulation parameters
parm_Q <- expand.grid(seed=1:bigB,
                    n=ns,
                    Q_period = four_period,
                    g_period = two_period)
parm_g <- expand.grid(seed=1:bigB,
                    n=ns,
                    Q_period = two_period,
                    g_period = four_period)
parm_g <- parm_g[-which(parm_g$g_period == 0.5 | parm_g$g_period == 10), ]

parm <- rbind(parm_Q, parm_g)

# directories to save in 
saveDir <- "~/haltmle.sim/out/"
scratchDir <- "~/haltmle.sim/scratch/"

# get the list size #########
if (args[1] == 'listsize') {
  cat(nrow(parm))
  # for testing, useful to just run the first job
  # to make sure everything saves correctly
  # cat(1)
}

make_sin <- function(n, Qp, gp){
  w1 <- runif(n, 0, 2*pi)
  # variation norm = 0.8*Qp
  Q0 <- 0.4*sin(Qp*w1) + 0.5 
  # variation norm = 0.8*gp
  g0 <- 0.4*sin(gp*w1) + 0.5

  A <- rbinom(n ,1, g0)
  Y <- rbinom(n, 1, Q0)

  return(list(W = data.frame(W1 = w1), A = A, Y = Y))
}

# execute prepare job ##################
if (args[1] == 'prepare') {
  # for(i in 1:nrow(parm)){
  # }
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

    set.seed(parm$seed[i])
    dat <- make_sin(n=parm$n[i], Qp = parm$Q_period[i], gp = parm$g_period[i])

    algo <- "SL.hal9001"
        
    # fit super learner with all algorithms
    out <- get_all_ates(Y = dat$Y, A = dat$A, W = dat$W, compute_superlearner = FALSE,
                        V = 6, learners = algo, remove_learner = NULL,
                        which_dr_tmle = c("SL.hal9001", "cv_SL.hal9001"))    

    save(out, file=paste0(saveDir,"sin_n=",parm$n[i],"_seed=",parm$seed[i],
                          "_Qp=",parm$Q_period[i],"_gp=",parm$g_period[i],".RData"))
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


