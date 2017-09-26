# devtools::document("~/Dropbox/R/haltmle.sim")
# install.packages("~/Dropbox/R/haltmle.sim", type = "source", repos = NULL)
# q("no")
library(haltmle.sim)
library(testthat)
context("Testing bivariate functions")

test_that("All bivariate functions work properly", {
	# declare all univariate functions
	allBivariateFunctions <- c("linBiv","polyBiv","sinBiv","jumpBiv",
                               "dNormAddBiv","dNormMultBiv","polyJumpBiv",
                               "sinJumpBiv","dNormMultJumpBiv","linSplineBiv")
	# small data set size
	n <- 50
	binW <- rbinom(n, 1, 0.5)
	contW <- rnorm(n)
	# test each one works when getting parameters and calling function
	for(f in allBivariateFunctions){
		# get parameters
		thisParm <- do.call(paste0(f,"Parm"), args = list(x1 = binW, x2 = contW))
		# call function with parameters 
		fOut <- do.call(f, args = c(list(x1 = binW, x2 = contW), thisParm))
		expect_true(sum(is.na(fOut))==0)
		expect_true(is.numeric(fOut))
	}	
})