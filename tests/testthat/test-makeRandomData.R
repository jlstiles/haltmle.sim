devtools::document("~/Dropbox/R/haltmle.sim")
install.packages("~/Dropbox/R/haltmle.sim", type = "source", repos = NULL)
q("no")
library(haltmle.sim)
library(testthat)
context("Testing makeRandomData functions")

test_that("makeRandomData works properly (25 calls)", {
	set.seed(1235)
	# small data set size
	n <- 50
	# number of tests
	nTest <- 10
	for(i in seq(nTest)){
		grbg <- makeRandomData(n = 50, maxD = i)
	}

})