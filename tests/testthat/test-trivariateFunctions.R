# devtools::document("~/Dropbox/R/haltmle.sim")
# install.packages("~/Dropbox/R/haltmle.sim", type = "source", repos = NULL)
# q("no")
library(haltmle.sim)
library(testthat)
context("Testing trivariate functions")

test_that("All trivariate functions work properly", {
	# declare all univariate functions
	allTrivariateFunctions <- c("linTri", "polyTri", "sinTri", "jumpTri",
	                            "linJump2Tri","linJump1Tri","polyJump2Tri",
	                            "polyJump1Tri","sinJump2Tri","sinJump1Tri",
	                            "dNormAddTri","dNormMultTri","dNormMultJump2Tri",
	                            "dNormMultJump1Tri","pLogisAddTri")
	# small data set size
	n <- 50
	binW <- rbinom(n, 1, 0.5)
	contW <- rnorm(n)
	contW2 <- runif(n)

	# test each one works when getting parameters and calling function
	for(f in allTrivariateFunctions){
		# get parameters
		thisParm <- do.call(paste0(f,"Parm"), args = list(x1 = binW, x2 = contW, x3 = contW2))
		# call function with parameters 
		fOut <- do.call(f, args = c(list(x1 = binW, x2 = contW, x3 = contW2), thisParm))
		expect_true(sum(is.na(fOut))==0)
		expect_true(is.numeric(fOut))
	}	
})