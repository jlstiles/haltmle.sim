# devtools::document("~/Dropbox/R/haltmle.sim")
# install.packages("~/Dropbox/R/haltmle.sim", type = "source", repos = NULL)
# q("no")
library(haltmle.sim)
library(testthat)
context("Testing quadravariate functions")

test_that("All error distribution functions work properly", {
	# declare all univariate functions
	allErrorDistributions <- c("normalErr","uniformErr","gammaErr","normalErrAW","uniformErrAW")
	# small data set size
	n <- 50
	binW <- rbinom(n, 1, 0.5)
	contW <- rnorm(n)
	contW2 <- runif(n)
	binW2 <- rbinom(n,1,0.5)
	AW <- cbind(binW, contW, contW2, binW2)

	# test each one works when getting parameters and calling function
	for(f in allErrorDistributions){
		# get parameters of error function
		errParm <- do.call(paste0(f, "Parm"), args = list())
		# evaluate error function
		errOut <- do.call(f, args = c(list(AW=AW, n=n), errParm))		
		expect_true(sum(is.na(errOut))==0)
		expect_true(is.numeric(errOut))
	}	
})