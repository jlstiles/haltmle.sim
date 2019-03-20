# Generate a random distribution with max vars 8, up to 4-way interactions will be randomly created with
# these variables. a randomly drawn number to skewing the pscore distribution, 100(1 - pos)% of pscores
# fall within minG0 and 1 - minG0
exD = makeRandomDataT(n=1000, maxD=8, minObsA = 300, minG0 = .1, pos = 0.05, skewing = c(-2,2))

mean(exD$A$A)

# number of iterations to control positivity-max is 20
exD$its
exD$skewage*.8^exD$its

# pscores
hist(exD$g0, 100)

# barQ
hist(exD$Q0, 100)

# max and min pscore, new then old
c(max(exD$g0), min(exD$g0))

#remake the data set according to the same specs randomly chosen!
remakeRandomData(100, exD)
