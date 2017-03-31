#' remakeRandomData
#' 
#' Given an object output by \code{makeRandomData}, make another data set with the same
#' distribution. 
#' 
#' @param n A \code{numeric} sample size
#' @param maxD A \code{numeric} indicating maximum number of covariates
#' @param distW.uncor A \code{vector} of \code{characters} that are functions in the global environment that 
#' generate covariates. These functions should have an associated parameter function with \code{"Parm"} added 
#' to the end of their name. See the format of the functions provided to understand their structure. 
#' @param distW.cor Same as above, but generates a correlated covariate
#' @param funcG0.uni A \code{vector} of \code{characters} that are functions in the global environment that
#' are used to generate main terms for logit of the propensity score.
#' @param funcG0.biv Ditto above, but for bivariate functions.
#' @param funcG0.tri Ditto above, but for trivariate functions. 
#' @param funcQ0.uni Ditto \code{funcG0.uni}, but for the outcome regression
#' @param funcQ0.biv Ditto
#' @param funcQ0.tri Ditto
#' @param errY A \code{vector} of \code{characters} that are functions in the global environment that 
#' are used to generate error terms for the outcome regression.
#' @param minG0 The minimum value for the propensity score (default 0.01). 
#' 
#' @return An object of class \code{"makeRandomData"} with the following entries
#' \item{W}{A matrix of covariates}
#' \item{A}{A vector of binary treatments}
#' \item{Y}{A vector of continuously valued outcome}
#' \item{distW}{A list containing relevant information needed to reproduce data sets}
#' \item{fnG0}{A list of lists containing relevant information needed to reproduce data sets}
#' \item{fnQ0}{A list of lists containing relevant information needed to reproduce data sets}
#' \item{distErrY}{A list containing relevant information needed to reproduce data sets}
#' @export

remakeRandomData <- function(n, object, setA = NULL, ...){
	# draw random number of covariates
	D <- ncol(object$W)

	#----------------------------------------------------------------------
	# Simulate W	
	#----------------------------------------------------------------------
	# initialize empties	
	W <- matrix(nrow = n, ncol = D)
	distW <- vector(mode = "list", length = D)
	for(d in 1:D){
		if(d == 1){
			W[,d] <- do.call(object$distW[[d]]$fn, args = c(list(n=n), object$distW[[d]]$parm))			
		}else{
			W[,d] <- do.call(object$distW[[d]]$fn, args = c(list(n=n, x = W[,d-1]), object$distW[[d]]$parm))
		}
	}

	#----------------------------------------------------------------------
	# Simulate propensity -- only if setA == NULL
	#----------------------------------------------------------------------
	if(is.null(setA)){
		# draw random number of main terms
		Mg1 <- length(object$fnG0$uni)
		Mg2 <- length(object$fnG0$biv)
		Mg3 <- length(object$fnG0$tri)

		# initialize empty
		logitg0 <- rep(0, n)

		# univariate
		for(m in 1:Mg1){
			fOut <- do.call(object$fnG0$uni[[m]]$fn, args = c(list(x = W[,m]), object$fnG0$uni[[m]]$parm))
			# add to current logitg0
			logitg0 <- logitg0 + fOut
		}

		# two-way interactions
		if(Mg2 > 0){
			for(m in 1:Mg2){
				# call function with parameters
				fOut <- do.call(object$fnG0$biv[[m]]$fn, 
				                args = c(list(x1 = W[,object$fnG0$biv[[m]]$whichColsW[1]],
				                              x2 = W[,object$fnG0$biv[[m]]$whichColsW[2]]),
				                object$fnG0$biv[[m]]$parm))
				# add to current logitg0
				logitg0 <- logitg0 + fOut
			}
		}

		#trivariate
		if(Mg3 > 0){
			for(m in 1:Mg3){
				# call function with parameters
				fOut <- do.call(object$fnG0$tri[[m]]$fn, 
				                args = c(list(x1 = W[,object$fnG0$tri[[m]]$whichColsW[1]],
				                              x2 = W[,object$fnG0$tri[[m]]$whichColsW[2]],
				                              x3 = W[,object$fnG0$tri[[m]]$whichColsW[3]]),
				                object$fnG0$tri[[m]]$parm))# save output in list
				# add to current logitg0
				logitg0 <- logitg0 + fOut
			}
		}

		# correct for positivity violations
		logitg0 <- logitg0/object$divideLogitG0
		
		# simulate A
		A <- rbinom(n, 1, plogis(logitg0))
	}else{
		A <- rep(setA, n)
		logitg0 <- Inf
	}
	# matrix with A and W
	AW <- cbind(A, W)

	#----------------------------------------------------------------------
	# Simulate Y
	#----------------------------------------------------------------------
	# draw random number of main terms between 2 and D + 1, where we set 
	# 2 to be the minimum so that we ensure there is confounding. 
	MQ1 <- length(object$fnQ0$uni)
	MQ2 <- length(object$fnQ0$biv)
	MQ3 <- length(object$fnQ0$tri)
	
	# empty 
	Q0 <- rep(0, n)

	# main terms
	for(m in 1:MQ1){
		fOut <- do.call(object$fnQ0$uni[[m]]$fn, args = c(list(x = AW[,m]), object$fnQ0$uni[[m]]$parm))
		# add 
		Q0 <- Q0 + fOut
	}

	# two-way interactions
	if(MQ2 > 0){
		for(m in 1:MQ2){
			# call function with parameters 
			fOut <- do.call(object$fnQ0$biv[[m]]$fn, 
			                args = c(list(x1 = AW[,object$fnQ0$biv[[m]]$whichColsAW[1]], 
			                              x2 = AW[,object$fnQ0$biv[[m]]$whichColsAW[2]]),
			                object$fnQ0$biv[[m]]$parm))
			# add
			Q0 <- Q0 + fOut
		}
	}

	# three-way interactions 
	if(MQ3 > 0){
		for(m in 1:MQ3){
			# call function with parameters
			fOut <- do.call(object$fnQ0$tri[[m]]$fn, 
			                args = c(list(x1 = AW[,object$fnQ0$tri[[m]]$whichColsAW[1]], 
			                              x2 = AW[,object$fnQ0$tri[[m]]$whichColsAW[2]],
			                              x3 = AW[,object$fnQ0$tri[[m]]$whichColsAW[3]]),
			                object$fnQ0$tri[[m]]$parm))			
			# add
			Q0 <- Q0 + fOut
		}
	}


	# Drawing an error function
	# evaluate error function
	errOut <- do.call(object$distErrY$fn, args = c(list(W=W, n=n), object$distErrY$parm))

	# compute Y
	Y <- Q0 + errOut

	return(list(W = W, A = A, Y = Y, Q0 = Q0, g0 = plogis(logitg0), err = errOut))
}