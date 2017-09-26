devtools::document("~/Dropbox/R/haltmle.sim")
install.packages("~/Dropbox/R/haltmle.sim", type = "source", repos = NULL)
q("no")
library(haltmle.sim)
library(testthat)
context("Testing covariate functions")

test_that("All covariate distribution functions work properly", {
	# declare all univariate functions
	allCovariateDistributions <- c("uniformW","normalW","bernoulliW","binomialW","gammaW",
                                     "normalWCor","bernoulliWCor","uniformWCor", "gammaPointMassW",
                                     "binomialFracW","normalPointMassW")
	# small data set size
	n <- 50

	# test each one works when getting parameters and calling function
	for(f in allCovariateDistributions){
		# with W = NULL
		thisParm <- do.call(paste0(f,"Parm"), args = list(W = NULL))
		# generate covariate
		W <- do.call(f, args = c(list(n=n, W = NULL), thisParm))
		expect_true(sum(is.na(W))==0)
		expect_true(is.numeric(W))

		# with non-null W
		thisParm <- do.call(paste0(f,"Parm"), args = list(W = NULL))
		# generate covariate
		W <- do.call(f, args = c(list(n=n, W = data.frame(rnorm(n))), thisParm))
	}	
})