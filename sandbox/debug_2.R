install.packages("~/Dropbox/R/haltmle.sim", type = "source", repos = NULL)
q("no")

# # load packages
library(arm)
library(plyr)
library(gam)
library(caret)
library(haltmle.sim)
library(Rsolnp)
library(future)
library(cvma)
library(hal9001)
library(drtmle)
library(SuperLearner)
# full parm


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
# exe

i <- 1
# full parm
ns <- c(200, 1000, 5000)
ns <- c(200, 1000, 5000)
bigB <- 1000


# # simulation parameters
parm <- expand.grid(seed=1:bigB,
                    n=ns)


set.seed(parm$seed[i])
dat <- make_ks(n=parm$n[i])

# algo <- c("SL.hal9001",
#           "SL.glm",
#           "SL.bayesglm", 
#           "SL.earth",
#           "SL.step.interaction",
#           "SL.gam", 
#           "SL.dbarts.mod",
#           "SL.gbm.caretMod",
#           "SL.rf.caretMod",
#           "SL.rpart.caretMod", 
#           "SL.mean",
#           "SL.kernelKnn")
algo <- c("SL.glm","SL.mean")
# fit super learner with all algorithms
debug(get_all_ates)
out <- get_all_ates(Y = dat$Y, A = dat$A, W = dat$W, 
                V = 6, learners = algo, remove_learner = NULL,
                which_dr_tmle = c("full_sl","cv_full_sl"))

debug(estimate_nuisance)
debug(get_ate_cv_Q_pred)



#-----
# sin debug
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
i <- 1
parm <- rbind(parm_Q, parm_g)

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

dat <- make_sin(n=parm$n[i], Qp = parm$Q_period[i], gp = parm$g_period[i])

algo <- "SL.glm"
    
# fit super learner with all algorithms
debug(get_all_ates)
out <- get_all_ates(Y = dat$Y, A = dat$A, W = dat$W, compute_superlearner = FALSE,
                    V = 6, learners = algo, remove_learner = NULL,
                    which_dr_tmle = c("SL.glm", "cv_SL.glm"))    
debug(estimate_nuisance)
debug(get_ate_cv_Q_pred)