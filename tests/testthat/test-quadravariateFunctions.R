# devtools::document("~/Dropbox/R/haltmle.sim")
# install.packages("~/Dropbox/R/haltmle.sim", type = "source", repos = NULL)
# q("no")
library(haltmle.sim)
library(testthat)
context("Testing quadravariate functions")

test_that("All trivariate functions work properly", {
	# declare all univariate functions
	allQuadvariateFunctions <- c("linQuad", "polyQuad", "sinQuad", "jumpQuad",
	                            "linJump2Quad","linJump1Quad","polyJump2Quad",
	                            "polyJump1Quad","sinJump2Quad","sinJump1Quad",
	                            "dNormAddQuad","dNormMultQuad","dNormMultJump2Quad",
	                            "dNormMultJump1Quad","pLogisAddQuad")
	# small data set size
	n <- 50
	binW <- rbinom(n, 1, 0.5)
	contW <- rnorm(n)
	contW2 <- runif(n)
	binW2 <- rbinom(n,1,0.5)

	# test each one works when getting parameters and calling function
	for(f in allQuadvariateFunctions){
		# get parameters
		thisParm <- do.call(paste0(f,"Parm"), 
		                    args = list(x1 = binW, x2 = contW, x3 = contW2, x4 = binW2))
		# call function with parameters 
		fOut <- do.call(f, args = c(list(x1 = binW, x2 = contW, x3 = contW2, x4 = binW2), thisParm))
		expect_true(sum(is.na(fOut))==0)
		expect_true(is.numeric(fOut))
	}	
})