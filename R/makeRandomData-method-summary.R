#' summary.makeRandomData
#' 
#' A method to summarize output of makeRandomData
#' @export
#' 

summary.makeRandomData <- function(object, N = 1e6, seed = 1234, ...){
	n <- length(object$Y[,1])
	D <- dim(object$W)[2]
	# minimum observed g_0(A | W)
	minObsG0 <- min(object$g0)
	# number of terms propensity
	mainTermsG0 <- length(object$fnG0$uni)
	bivTermsG0 <- length(object$fnG0$biv)
	triTermsG0 <- length(object$fnG0$tri)
	# number of terms outcome
	mainTermsQ0 <- length(object$fnQ0$uni)
	bivTermsQ0 <- length(object$fnQ0$biv)
	triTermsQ0 <- length(object$fnQ0$tri)
	# number binary covariates
	uniqVals <- apply(object$W, 2, function(x){length(unique(x))})
	numBinW <- sum(uniqVals == 2)
	# number ordered categorical
	numCatW <- sum(uniqVals < n)
	# number continuous
	numContW <- sum(uniqVals == n)
	
	# helper function to get table of each function 
	# x is input as the object$fnX$dim and allF is input as
	# object$funcX.D
	.getTableFn <- function(x, allF, Q = FALSE, uni = FALSE){
		allFuncUsed <- unlist(lapply(x, "[[", "fn"))
		tmp <- rep(0, length(allF))	
		if(uni){
			if(Q){
				allFuncUsed <- allFuncUsed[2:length(allFuncUsed)]
			}
			for(i in 1:length(allFuncUsed)){
				ind <- which(allF == allFuncUsed[i])
				tmp[ind] <- tmp[ind] + (uniqVals[i] == n)
			}
		}else{
			for(i in 1:length(allFuncUsed)){
				ind <- which(allF == allFuncUsed[i])
				if(!Q){
					tmp[ind] <- tmp[ind] + (any(uniqVals[x[allFuncUsed[i]]$whichColsW] == n))		
				}else{
					tmp[ind] <- tmp[ind] + (any(c(2,uniqVals)[x[[i]]$whichColsAW] == n))
				}
			}
		}

		if(!Q){
			names(tmp) <- paste0(allF,"G0")
		}else{
			names(tmp) <- paste0(allF,"Q0")			
		}
		return(tmp)
	}

	allFunG0.uni <- .getTableFn(x = object$fnG0$uni, allF = object$funcG0.uni, uni = TRUE)
	allFunG0.biv <- .getTableFn(x = object$fnG0$biv, allF = object$funcG0.biv)
	allFunG0.tri <- .getTableFn(x = object$fnG0$tri, allF = object$funcG0.tri)
	
	allFunQ0.uni <- .getTableFn(x = object$fnQ0$uni, allF = object$funcQ0.uni, Q = TRUE, uni = TRUE)
	allFunQ0.biv <- .getTableFn(x = object$fnQ0$biv, allF = object$funcQ0.biv, Q = TRUE)
	allFunQ0.tri <- .getTableFn(x = object$fnQ0$tri, allF = object$funcQ0.tri, Q = TRUE)

	# noisy covariates
	.numNoisyW <- function(object.fn, Q = FALSE){
		whichWUni <- 1:length(object.fn$uni)
		whichWBiv <- unique(unlist(lapply(object.fn$biv, "[[", "whichColsW")))
		whichWTri <- unique(unlist(lapply(object.fn$tri, "[[", "whichColsW")))
		usedW <- length(unique(c(whichWUni, whichWBiv, whichWTri)))
		if(Q) usedW <- usedW - 1  # A is always used
		nNoise <- D - usedW
		return(nNoise)
	}

	numNoisyG0 <- .numNoisyW(object.fn = object$fnG0)
	numNoisyQ0 <- .numNoisyW(object.fn = object$fnQ0, Q = TRUE)
	
	# generate big observed data set
	set.seed(seed)
	bigObs <- remakeRandomData(n = N, object = object)
	# generate big setA data set
	set.seed(seed)
	bigSet_1 <- remakeRandomData(n = N, object = object, setA = 1)
	bigSet_0 <- remakeRandomData(n = N, object = object, setA = 0)

	# r-squared for \bar{Q}_0(1,W)
	r2 <- 1 - mean((bigObs$Y - bigObs$Q0)^2)/var(bigObs$Y)
	# true ATE
	truth <- mean(bigSet_1$Y) - mean(bigSet_0$Y)
	# marginal observed P(A = 1)
	PA1 <- mean(bigObs$A)
	# observed causal effect
	obsATE <- mean(bigObs$Y[bigObs$A==1]) - mean(bigObs$Y[bigObs$A==0])
	# variance of the eff ic
	effIC <- as.numeric(2*(bigObs$A == 1) - 1)/(bigObs$A*bigObs$g0 + (1-bigObs$A)*(1-bigObs$g0)) *
								(bigObs$Y - bigObs$Q0) + (bigSet_1$Q0 - bigSet_0$Q0) - truth
	varEffIC <- var(effIC)

	# covariate correlations
	corMatW <- cor(bigSet$W)
	corrW <- as.vector(corMatW)[as.vector(lower.tri(corMatW))]
	nCorr1 <- sum(corrW < 0.1)
	nCorr2 <- sum(corrW >= 0.1 & corrW < 0.4)
	nCorr3 <- sum(corrW >= 0.4 & corrW < 0.6)
	nCorr4 <- sum(corrW > 0.6)

	# error distribution
	corErr <- cor(cbind(bigSet$err,bigSet$W))[,1]
	nCorr1 <- sum(corrW < 0.1)
	nCorr2 <- sum(corrW >= 0.1 & corrW < 0.4)
	nCorr3 <- sum(corrW >= 0.4 & corrW < 0.6)
	nCorr4 <- sum(corrW > 0.6)

	# measure of error dependence on W
	# by default package only includes error distributions that 
	# are correlated with the first component of W; more generally, 
	# we might later add functions allowed to depend on multiple components
	# of W, in which case we will want to add more correlation measures 
	# between W and errors.
	# Also, may want to consider bptest in lmtest package?
	corSqErrW <- cor(bigSet$err^2,bigSet$W[,1])

	out <- list(
     n = length(object$A), sumA1 = sum(object$A==1), D = D, minObsG0 = minObsG0,
     mainTermsG0 = mainTermsG0, bivTermsG0 = bivTermsG0, triTermsG0 = triTermsG0,
     mainTermsQ0 = mainTermsQ0, bivTermsQ0 = bivTermsQ0, triTermsQ0 = triTermsQ0,
     fnG0.table = list(allFunG0.uni, allFunG0.biv, allFunG0.tri),
     fnQ0.table = list(allFunQ0.uni, allFunQ0.biv, allFunQ0.tri),
     numBinW = numBinW, numCatW = numCatW, numContW = numContW,
     numNoisyG0 = numNoisyG0, numNoisyQ0 = numNoisyQ0, 
     PA1 = PA1, r2 = r2, truth = truth, varEffIC = varEffIC, obsATE = obsATE,
     nCorr1 = nCorr1, nCorr2 = nCorr2, nCorr3 = nCorr3, nCorr4 = nCorr4, 
     corSqErrW = corSqErrW, errDist = object$distErrY
  	)
  	return(out)
}