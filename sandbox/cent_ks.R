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
# ns <- c(200, 1000, 5000)
# ns <- c(200, 1000, 5000)
# bigB <- 1000


# # # simulation parameters
# parm <- expand.grid(seed=1:bigB,
#                     n=ns)

# parm <- find_missing_files(tag = "ks",
#                            # parm needs to be in same order as 
#                            # file saves -- should make this more general...
#                            parm = c("n", "seed"),
#                            parm_values = list(seed = 1:1000,
#                                               n = c(200,1000,5000)))
# save(parm, file = "~/haltmle.sim/scratch/remain_ks_sims.RData")

load("~/haltmle.sim/scratch/remain_ks_sims.RData")


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

make_ks <- function(n){
  z1 <- rnorm(n)
  z2 <- rnorm(n)
  z3 <- rnorm(n)
  z4 <- rnorm(n)

  w1 <- exp(z1/2)
  w2 <- z2/(1+exp(z1)) + 10
  w3 <- (z1*z3/25 + 0.6)^3
  w4 <- (z2 + z4 + 20)^2

  Y <- 210 + 27.4*z1 + 13.7*z2 + 13.7*z3 + 13.7*z4 + rnorm(n)
  g0 <- plogis(-z1 + 0.5*z2 - 0.25*z3 - 0.1*z4)
  A <- rbinom(n, 1, g0)
  return(list(Y = Y, A = A, W = data.frame(W1 = w1, W2 = w2, W3 = w3, W4 = w4)))
}
# execute prepare job ##################
if (args[1] == 'prepare') {

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
    dat <- make_ks(n=parm$n[i])

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
    out <- get_all_ates(Y = dat$Y, A = dat$A, W = dat$W, 
                        V = 6, learners = algo, remove_learner = "SL.hal9001")

    save(out, file=paste0(saveDir,"ks_n=",parm$n[i],"_seed=",parm$seed[i],
                          ".RData"))
    }
}

# merge job ###########################
if (args[1] == 'merge') {
  format_result <- function(out){
    tmle_os_list <- lapply(out, function(l){
      # browser()
      cv_est <- grepl("cv_", names(l))
      for(i in 1:length(l)){
        l[[i]] <- data.frame(as.list(l[[i]]))
      }
      for(i in 1:length(l)){
        if(cv_est[i]){
          l[[i]]$cv_ci_l <- l[[i]]$est - qnorm(0.975)*l[[i]]$se
          l[[i]]$cv_ci_u <- l[[i]]$est + qnorm(0.975)*l[[i]]$se
          l[[i]]$cov_cv_ci <- l[[i]]$cv_ci_l < truth & l[[i]]$cv_ci_u > truth
        }else{
          l[[i]]$ci_l <- l[[i]]$est - qnorm(0.975)*l[[i]]$se
          l[[i]]$ci_u <- l[[i]]$est + qnorm(0.975)*l[[i]]$se
          l[[i]]$cov_ci <- l[[i]]$ci_l < truth & l[[i]]$ci_u > truth
          l[[i]]$cv_ci_l <- l[[i]]$est - qnorm(0.975)*l[[i+1]]$se
          l[[i]]$cv_ci_u <- l[[i]]$est + qnorm(0.975)*l[[i+1]]$se
          l[[i]]$cov_cv_ci <- l[[i]]$cv_ci_l < truth & l[[i]]$cv_ci_u > truth
        }
      }
      return(l)
    })
    return(tmle_os_list)
  }

  all_files <- list.files("~/haltmle.sim/out")
  ks_files <- all_files[grepl("ks",all_files)]
  logistic_tmle_rslt <- matrix(nrow = length(ks_files), ncol = 182 + 1)
  linear_tmle_rslt <- matrix(nrow = length(ks_files), ncol = 182 + 1)
  onestep_rslt <- matrix(nrow = length(ks_files), ncol = 182 + 1)
  drtmle_rslt <- matrix(nrow = length(ks_files), ncol = 26 + 1)
  for(i in seq_along(ks_files)){
    # get sample size
    this_n <- as.numeric(strsplit(strsplit(ks_files[i], "_")[[1]][2], "n=")[[1]][2])
    # load file
    load(ks_files[i])
    # format this file
    tmp <- format_result(out)
    # add results to rslt
    logistic_tmle_rslt[i,] <- c(this_n, unlist(tmp[[1]]))
    linear_tmle_rslt[i,] <- c(this_n,unlist(tmp[[2]]))
    onestep_rslt[i,] <- c(this_n,unlist(tmp[[3]]))
    drtmle_rslt[i,] <- c(this_n,unlist(tmp[[4]]))
  }
  col_names_1 <- c("n", names(unlist(tmp[[1]],use.names = TRUE)))
  col_names_2 <- c("n", names(unlist(tmp[[4]],use.names = TRUE)))
  logistic_tmle_rslt <- data.frame(logistic_tmle_rslt)
  colnames(logistic_tmle_rslt) <- col_names_1
  linear_tmle_rslt <- data.frame(linear_tmle_rslt)
  colnames(linear_tmle_rslt) <- col_names_1
  onestep_rslt <- data.frame(onestep_rslt)
  colnames(onestep_rslt) <- col_names_1
  drtmle_rslt <- data.frame(drtmle_rslt)
  colnames(drtmle_rslt) <- col_names_2

  # bias
  by(logistic_tmle_rslt, logistic_tmle_rslt$n, function(x){
    colMeans(x[,c("full_sl.cov_ci","full_sl.cov_cv_ci","cv_full_sl.cov_cv_ci",
                  "SL.hal9001.cov_ci","SL.hal9001.cov_cv_ci","cv_SL.hal9001.cov_cv_ci"
                  )])
  })
  by(linear_tmle_rslt, linear_tmle_rslt$n, function(x){
    colMeans(x[,c("full_sl.cov_ci","full_sl.cov_cv_ci","cv_full_sl.cov_cv_ci",
                  "SL.hal9001.cov_ci","SL.hal9001.cov_cv_ci","cv_SL.hal9001.cov_cv_ci"
                  )])
  })
  by(onestep_rslt, onestep_rslt$n, function(x){
    colMeans(x[,c("full_sl.cov_ci","full_sl.cov_cv_ci","cv_full_sl.cov_cv_ci",
                  "SL.hal9001.cov_ci","SL.hal9001.cov_cv_ci","cv_SL.hal9001.cov_cv_ci"
                  )])
  })
  by(drtmle_rslt, drtmle_rslt$n, function(x){
    colMeans(x[,c("full_sl.cov_ci","full_sl.cov_cv_ci","cv_full_sl.cov_cv_ci",
                  "SL.hal9001.cov_ci","SL.hal9001.cov_cv_ci","cv_SL.hal9001.cov_cv_ci"
                  )])
  })

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


