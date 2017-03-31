#' makeRandomData
#' 
#' Simulate a random data generating distribution. See Details to see how it's done. 
#' 
#' 
#' Draw random data generating distribution. The code simulates as follows: \cr
#' Simulate W \itemize{ 
#' \item Choose \code{D}, a random number of covariates from Uniform(1,maxD)
#' \item Randomly choose \code{d1}, the distribution for first covariate, from \code{distW.uncor}
#' \item Call \code{paste0(d1,".Parm")} (which is assumed to exist in the global environment) to get parameters for \code{d1}
#' \item Call \code{d1} with parameters chosen in previous step to generate first covariate
#' \item Randomly choose \code{d2}, the distribution for second covariate, from \code{c(distW.uncor, distW.cor)} (now including correlated distributions).
#' \item Call \code{paste0(d2,".Parm")} to get parameters for \code{d2}
#' \item Call \code{d2} with parameters chosen in previous step and first covariate (to possibly induce correlation with previous covariate) to generate second covariate. 
#' \item Continue until \code{maxD} is reached.
#' }
#' 
#' Simulating A \itemize{
#' \item Draw \code{MG1}, a random number of main terms for propensity score, from Uniform(1,maxD)
#' \item For \code{i} = 1,...,\code{MG1}, draw \code{f} a random function from \code{funcG0.uni}
#' \item Call \code{paste0(f,"Parm")} (assumed to exist) to generate parameters for \code{f}
#' \item Call \code{f} with \code{x = W[,i]} to generate \code{i}-th main term. 
#' \item If \code{D > 1}, draw \code{MG2}, a random number of bivariate interactions for propensity, from Uniform(1,\code{MG1-1})
#' \item For \code{i} = 1,...,\code{MG2}, draw \code{f} a random function from \code{funcG0.biv}
#' \item Call \code{paste0(f,"Parm")} (assumed to exist) to generate parameters for \code{f}
#' \item Draw \code{j} and \code{k}, two random columns of \code{W} 
#' \item Call \code{f} with \code{x1 = W[,j], x2 = W[,k]} to generate \code{i}-th bivariate interaction term.
#' \item If \code{D > 2}, draw \code{MG3}, a random number of trivariate interactions for propensity, from Uniform(1,\code{MG2-1})
#' \item For \code{i} = 1,...,\code{MG3}, draw \code{f} a random function from \code{funcG0.tri}
#' \item Call \code{paste0(f,"Parm")} (assumed to exist) to generate parameters for \code{f}
#' \item Draw \code{j},\code{k},\code{l}, three random columns of \code{W} 
#' \item Call \code{f} with \code{x1 = W[,j], x2 = W[,k], x3 = W[,l]} to generate \code{i}-th trivariate interaction term.
#' \item Sum together all main, bivariate, and trivariate terms to get the logit propensity score.
#' \item Divide the logit propensity score by 1.01 until the minimum propensity score is bigger than \code{minG0}.
#' \item Draw A from Bernoulli distribution with conditional probability equal to the adjusted propensity score. 
#' }
#' 
#' Simulating Y \itemize{
#' \item Draw \code{MQ1}, a random number of main terms for outcome regression score, from Uniform(2,maxD)
#' \item For \code{i} = 1,...,\code{MQ1}, draw \code{f} a random function from \code{funcQ0.uni}
#' \item Call \code{paste0(f,"Parm")} (assumed to exist) to generate parameters for \code{f}
#' \item Call \code{f} with \code{x = AW[,i]} to generate \code{i}-th main term, where AW is matrix with first column A, other columns W. 
#' \item If \code{D > 1}, draw \code{MQ2}, a random number of bivariate interactions for outcome regression, from Uniform(1,\code{MQ1-1})
#' \item For \code{i} = 1,...,\code{MQ2}, draw \code{f} a random function from \code{funcQ0.biv}
#' \item Call \code{paste0(f,"Parm")} (assumed to exist) to generate parameters for \code{f}
#' \item Draw \code{j} and \code{k}, two random columns of \code{W} 
#' \item Call \code{f} with \code{x1 = W[,j], x2 = W[,k]} to generate \code{i}-th bivariate interaction term.
#' \item If \code{D > 2}, draw \code{MQ3}, a random number of trivariate interactions for propensity, from Uniform(1,\code{MQ2-1})
#' \item For \code{i} = 1,...,\code{MQ3}, draw \code{f} a random function from \code{funcQ0.tri}
#' \item Call \code{paste0(f,"Parm")} (assumed to exist) to generate parameters for \code{f}
#' \item Draw \code{j},\code{k},\code{l}, three random columns of \code{W} 
#' \item Call \code{f} with \code{x1 = W[,j], x2 = W[,k], x3 = W[,l]} to generate \code{i}-th trivariate interaction term.
#' \item Sum together all main, bivariate, and trivariate terms to get the conditional mean outcome, \code{Q0}. 
#' \item Draw \code{errf}, arandom function from \code{errY} from which to draw errors
#' \item Call \code{paste0(errf,"Parm"} (assumed to exist) to generate random parameters for error distribution. 
#' \item Call \code{errf} with randomly chosen parameters to draw \code{e}, an error for the outcome.
#' \item Let \code{Y = Q0 + e}. 
#' }
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
#' \item{divideLogitG0}{A numeric of the scaling factor for the propensity}
#' @export

makeRandomData <- function(n, 
                           maxD,  
                           distW.uncor = c("uniformW","normalW","bernoulliW","binomialW","gammaW"), 
                           distW.cor = c("normalWCor","bernoulliWCor","uniformWCor"), 
                           funcG0.uni = c("linUni","polyUni","sinUni","jumpUni","pLogisUni",
                                          "dNormUni","qGammaUni","dNormMixUni"), 
                           funcG0.biv = c("linBiv","polyBiv","sinBiv","jumpBiv",
                                          "dNormAddBiv","dNormMultBiv"), 
                           funcG0.tri = c("linTri", "polyTri", "sinTri", "jumpTri"), 
                           funcQ0.uni = c("linUni","polyUni","sinUni","jumpUni","pLogisUni",
                                          "dNormUni","qGammaUni","dNormMixUni"), 
                           funcQ0.biv = c("linBiv","polyBiv","sinBiv","jumpBiv",
                                          "dNormAddBiv","dNormMultBiv"), 
                           funcQ0.tri = c("linTri","polyTri","sinTri","jumpTri"), 
                           errY = c("normalErr","uniformErr","gammaErr","normalErrW","uniformErrW"),
                           minG0 = 1e-2, 
                           ...){
	# draw random number of covariates
	D <- round(runif(1, 0.5, maxD+0.5))

	#----------------------------------------------------------------------
	# Simulate W	
	#----------------------------------------------------------------------
	# initialize empties	
	W <- matrix(nrow = n, ncol = D)
	distW <- vector(mode = "list", length = D)
	for(d in 1:D){
		# randomly sample distribution
		if(d==1){
			# sample distribution
			thisD <- sample(distW.uncor, 1)
			# generate parameters
			thisParm <- do.call(paste0(thisD,"Parm"), args = list())
			# generate covariate
			W[,d] <- do.call(thisD, args = c(list(n=n), thisParm))
		}else{
			# sample distribution
			thisD <- sample(c(distW.uncor,distW.cor), 1)
			# generate parameters
			thisParm <- do.call(paste0(thisD,"Parm"), args = list())
			# generate covariate
			W[,d] <- do.call(thisD, args = c(list(n=n, x = W[,d-1]), thisParm))
		}
		# save distributions
		distW[[d]]$fn <- thisD; distW[[d]]$parm <- thisParm
	}


	#----------------------------------------------------------------------
	# Simulate propensity
	#----------------------------------------------------------------------
	# draw random number of main terms
	Mg1 <- round(runif(1, 0.5, D + 0.5))

	# draw random number of two-way interaction terms
	Mg2 <- ifelse(Mg1 > 1, round(runif(1, 0.5, Mg1 - 0.5)), 0)

	# draw random number of three-way interaction terms
	Mg3 <- ifelse(Mg2 > 1, round(runif(1, 0.5, Mg2 - 0.5)), 0)

	# initialize empty
	logitg0 <- rep(0, n)

	# univariate
	uniG0 <- vector(mode="list", length = Mg1)
	for(m in 1:Mg1){
		# draw random function
		thisF <- sample(funcG0.uni, 1)
		# get parameters
		thisParm <- do.call(paste0(thisF,"Parm"), args = list(x = W[,m]))
		# call function with parameters 
		fOut <- do.call(thisF, args = c(list(x = W[,m]), thisParm))
		# save output in list
		uniG0[[m]] <- list(fn = thisF, parm = thisParm)
		# add to current logitg0
		logitg0 <- logitg0 + fOut
	}

	# two-way interactions
	if(Mg2 > 0){
		# empty list
		bivG0 <- vector(mode="list",length=Mg2)
		# all two-way column combinations
		comb <- combn(Mg1,2)
		# randomly sample Mg2 two-way interactions without replacement 
		combCols <- sample(1:ncol(comb),Mg2)
		for(m in 1:Mg2){
			# the two columns used for this interaction
			theseCols <- comb[,combCols[m]]
			# the random function to be used
			thisF <- sample(funcG0.biv, 1)
			# get parameters for function
			thisParm <- do.call(paste0(thisF,"Parm"), args = list(x1 = W[,theseCols[1]], x2 = W[,theseCols[2]]))
			# call function with parameters
			fOut <- do.call(thisF, args = c(list(x1 = W[,theseCols[1]], x2 = W[,theseCols[2]]),thisParm))
			# save output in list
			bivG0[[m]] <- list(fn = thisF, parm = thisParm, whichColsW = theseCols)
			# add to current logitg0
			logitg0 <- logitg0 + fOut
		}
	}else{
		bivG0 <- NULL
	}

	#trivariate
	if(Mg3 > 0){
		# empty list
		triG0 <- vector(mode="list",length=Mg3)
		# all three way choices of columns
		comb <- combn(Mg1,3)
		# randomly sample Mg3 three-way interactions without replacement 
		combCols <- sample(1:ncol(comb),Mg3)
		for(m in 1:Mg3){
			# the three columns used for this function
			theseCols <- comb[,combCols[m]]
			# the random function to be used
			thisF <- sample(funcG0.tri, 1)
			# get parameters for function
			thisParm <- do.call(paste0(thisF,"Parm"), args = list(x1 = W[,theseCols[1]], x2 = W[,theseCols[2]], x3=W[,theseCols[3]]))
			# call function with parameters
			fOut <- do.call(thisF, args = c(list(x1 = W[,theseCols[1]], x2 = W[,theseCols[2]], x3=W[,theseCols[3]]),thisParm))
			# save output in list
			triG0[[m]] <- list(fn = thisF, parm = thisParm, whichColsW = theseCols)
			# add to current logitg0
			logitg0 <- logitg0 + fOut
		}
	}else{
		triG0 <- NULL
	}

	# correct for positivity violations
	iter <- 0
	while(min(plogis(logitg0)) < minG0){
		iter <- iter + 1
		logitg0 <- logitg0/1.01^iter
	}
	divideLogitG0 <- 1.01^iter

	# simulate A
	A <- rbinom(n, 1, plogis(logitg0))

	# matrix with A and W
	AW <- cbind(A, W)

	#----------------------------------------------------------------------
	# Simulate Y
	#----------------------------------------------------------------------
	# draw random number of main terms between 2 and D + 1, where we set 
	# 2 to be the minimum so that we ensure there is confounding. 
	MQ1 <- round(runif(1, 1.5, D + 1.5))

	# draw random number of interaction terms
	MQ2 <- ifelse(MQ1 > 1, round(runif(1, 0.5, MQ1 - 0.5)), 0)
	MQ3 <- ifelse(MQ2 > 1, round(runif(1, 0.5, MQ2 - 0.5)), 0)
	
	# empty 
	Q0 <- rep(0, n)

	# main terms
	# empty
	uniQ0 <- vector(mode="list", length = MQ1)
	for(m in 1:MQ1){
		# randomly draw function
		thisF <- sample(funcQ0.uni, 1)
		# get parameters
		thisParm <- do.call(paste0(thisF,"Parm"), args = list(x = AW[,m]))
		# call function with parameters
		fOut <- do.call(thisF, args = c(list(x = AW[,m]), thisParm))
		# save results
		uniQ0[[m]] <- list(fn = thisF, parm = thisParm)
		# add 
		Q0 <- Q0 + fOut
	}

	# two-way interactions
	if(MQ2 > 0){
		bivQ0 <- vector(mode="list",length=MQ2)
		# all combinations of columns
		comb <- combn(MQ1,2)
		# randomly sample columns
		combCols <- sample(1:ncol(comb),MQ2)
		for(m in 1:MQ2){
			# what columns to use for this interaction
			theseCols <- comb[,combCols[m]]
			# randomly sample function
			thisF <- sample(funcQ0.biv,1)
			# get parameters
			thisParm <- do.call(paste0(thisF,"Parm"), args = list(x1 = AW[,theseCols[1]], x2 = AW[,theseCols[2]]))
			# call function with parameters 
			fOut <- do.call(thisF, args = c(list(x1 = AW[,theseCols[1]], x2 = AW[,theseCols[2]]),thisParm))
			# save output
			bivQ0[[m]] <- list(fn = thisF, parm = thisParm, whichColsAW = theseCols)
			# add
			Q0 <- Q0 + fOut
		}
	}else{
		bivQ0 <- NULL
	}

	# three-way interactions 
	if(MQ3 > 0){
		# empty
		triQ0 <- vector(mode="list",length=MQ3)
		# all three-way column combinations
		comb <- combn(MQ1,3)
		# randomly sample three choices of combinations without replacement
		combCols <- sample(1:ncol(comb),MQ3)
		for(m in 1:MQ3){
			# columns to use for this interaction
			theseCols <- comb[,combCols[m]]
			# randomly sample function
			thisF <- sample(funcQ0.tri,1)
			# get parameters 
			thisParm <- do.call(paste0(thisF,"Parm"), args = list(x1 = AW[,theseCols[1]], x2 = AW[,theseCols[2]], x3=AW[,theseCols[3]]))
			# call function with parameters
			fOut <- do.call(thisF, args = c(list(x1 = AW[,theseCols[1]], x2 = AW[,theseCols[2]], x3=AW[,theseCols[3]]),thisParm))
			# save output
			triQ0[[m]] <- list(fn = thisF, parm = thisParm, whichColsAW = theseCols)
			# add
			Q0 <- Q0 + fOut
		}
	}else{
		triQ0 <- NULL
	}


	# Drawing an error function
	# empty results
	errYList <- vector(mode = "list")
	# randomly draw error function
	errFn <- sample(errY, 1)
	# get parameters of error function
	errParm <- do.call(paste0(errFn, "Parm"), args = list())
	# evaluate error function
	errOut <- do.call(errFn, args = c(list(W=W, n=n), errParm))
	# save output
	errYList$fn <- errFn; errYList$parm <- errParm

	# compute Y
	Y <- Q0 + errOut

	out <- list(W = W, A = data.frame(A=A), Y = data.frame(Y=Y), distW = distW, fnG0 = list(uni = uniG0, biv = bivG0, tri = triG0),
		            fnQ0 = list(uni = uniQ0, biv = bivQ0, tri = triQ0), distErrY = errYList, divideLogitG0 = divideLogitG0,
		            Q0 = Q0, g0 = plogis(logitg0), funcG0.uni = funcG0.uni, funcG0.biv = funcG0.biv, funcG0.tri = funcG0.tri,
		            funcQ0.uni = funcQ0.uni, funcQ0.biv = funcQ0.biv, funcQ0.tri = funcQ0.tri)
	class(out) <- "makeRandomData"
	return(out)
}