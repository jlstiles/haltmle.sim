# Generate a random distribution with max vars 8, up to 4-way interactions will be randomly created with
# these variables. a randomly drawn number to skewing the pscore distribution, 100(1 - pos)% of pscores
# fall within minG0 and 1 - minG0

# right now it limits 2, 3 or 4 way interactions to be at most the number of main terms.
exD = makeRandomDataT(n=1000, maxD=10, minD = 1, minObsA = 300, minG0 = 0.05, pos = 0.05, skewing = c(-1,1))
# undebug(makeRandomDataT)
exD$its
exD$ct

mean(exD$A$A)
head(exD$W)
# number of iterations to control positivity-max is 20
exD$its
exD$skewage*.6^exD$its

# pscores
hist(exD$g0, 100)

# barQ
hist(exD$Q0, 100)

# max and min pscore, new then old
c(max(exD$g0), min(exD$g0))

#remake the data set according to the same specs randomly chosen!
exD_1 = remakeRandomDataT(1000, exD)
# undebug(remakeRandomDataT)
# pscores
hist(exD_1$g0, 100)
mean(exD_1$g0)
mean(exD_1$A)
hist(exD$g0, 100)
mean(exD$g0)

# barQ
hist(exD_1$Q0, 100)

# max and min pscore, new then old
c(max(exD_1$g0), min(exD_1$g0))

