# devtools::document("~/Dropbox/R/haltmle.sim")
# install.packages("~/Dropbox/R/haltmle.sim", type = "source", repos = NULL)
# q("no")
library(haltmle.sim)
library(testthat)
context("Testing univariate functions")

test_that("All univariate functions work properly", {
	# declare all univariate functions
	allUnivariateFunctions <- c("linUni","polyUni","sinUni","jumpUni","pLogisUni",
                                          "dNormUni","qGammaUni","dNormMixUni","cubicSplineUni",
                                          "linSplineUni")
	# small data set size
	n <- 50
	binW <- rbinom(n, 1, 0.5)
	contW <- rnorm(n)
	# test each one works when getting parameters and calling function
	for(f in allUnivariateFunctions){
		# get parameters
		thisParm <- do.call(paste0(f,"Parm"), args = list(x = binW))
		# call function with parameters 
		fOut <- do.call(f, args = c(list(x = binW), thisParm))
		expect_true(sum(is.na(fOut))==0)
		expect_true(is.numeric(fOut))
		
		# now for continuous covariates
		# get parameters
		thisParm <- do.call(paste0(f,"Parm"), args = list(x = contW))
		# call function with parameters 
		fOut <- do.call(f, args = c(list(x = contW), thisParm))
		expect_true(sum(is.na(fOut))==0)
		expect_true(is.numeric(fOut))
	}
})