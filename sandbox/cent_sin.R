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
ns <- c(200,1000,5000)
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
# colnames(parm)[3:4] <- c("Qp","gp")

# redo_parm <- find_missing_files(tag = "sin",
#                            # parm needs to be in same order as 
#                            # file saves -- should make this more general...
#                            parm = c("n", "seed", "Qp", "gp"),
#                            full_parm = parm)
# parm <- redo_parm
# save(parm, file = "~/haltmle.sim/scratch/remain_sin_sims.RData")
load("~/haltmle.sim/scratch/remain_sin_sims.RData")
names(parm)[3:4] <- c("Q_period","g_period")
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

  
}


