devtools::install_github("jlstiles/haltmle.sim")
devtools::install_github("jlstiles/Simulations")

library(haltmle.sim)
library(Simulations)

# JL random dgp maker, has controls for blip variance and positivity protection
ex = get.dgp(n = 1000, d = 4, minBV = .03, depth = 3, maxterms = 6, minterms = 3, mininters = 3, pos = .1,
             skewing = c(-5,5))

data = ex$DF
head(data)
W = data[,1:4]
A = data$A
Y = data$Y
alpha = .05
Qform = formula("Y ~ A*(W1+W2+W3+W4)")

# test if logistic plug-in using delta method regression works
info = LR.inference(W,A,Y,Qform, alpha = 0.05, simultaneous.inference = TRUE)

# does it cover ATE
(ex$ATE0>info[2]) & (ex$ATE0<info[3])
# does it cover blip variance
(ex$BV0>info[8]) & (ex$BV0<info[9])
# does it cover both simultaneously
((ex$ATE0>info[5]) & (ex$ATE0<info[6])) & ((ex$BV0>info[11]) & (ex$BV0<info[12]))

# pscore hist
hist(ex$PQn, 100)
# Qbar hist
hist(ex$PGn, 100)
# marginal prob of treatment
mean(ex$DF$A)


# modifying Davids function to protect against positivity
# and not give blank distributions
exD = makeRandomDataT(n=1000, maxD=8, minObsA = 300, minG0 = .1, skewing = c(-2,2))
exD1 = makeRandomData(n=1000, maxD=8, minObsA = 300, minG0 = .01)

mean(exD$A$A)
mean(exD1$A$A)

# number of iterations to control positivity-max is 20
exD$its
exD$skewage*.8^exD$its

# new then old pscores
hist(exD$g0, 100)
hist(exD1$g0, 100)

# new then old barQ
hist(exD$Q0, 100)
hist(exD1$Q0, 100)

# max and min pscore, new then old
c(max(exD$g0), min(exD$g0))
c(max(exD1$g0), min(exD1$g0))
