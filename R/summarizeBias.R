#' summarizeBias
#' 
#' Function to summarize bias of estimators across
#' different simulations.
#' 
#' @param result Result data frame
#' @param estNames Column name(s) of results data frame that are estimators
#' @param truthName Column name of results data frame that is the true ATE
#' @param ... Other arguments
#' 
#' @return A vector with the ordered bias of the estimators

summarizeBias <- function(result, estNames, truthName = "truth", ...){
	allBias <- apply(result[,c(estNames)], 2, function(x){
		median(abs(x/result[,truthName]-1),na.rm=TRUE)
	})
	ord <- order(allBias)
	allBias[ord]
}

#' summarizeCoverage
#' 
#' Function to summarize coverage of confidence intervals across all
#' simulations
#' 
#' @param result Result data frame
#' @param cilNames Column name(s) of results data frame that are lower confidence bounds
#' @param ciuNames Column name(s) of results data frame that are upper confidence bounds
#' @param truthName Column name of results data frame that is the true ATE
#' @param ... Other arguments
#' 
#' @return A vector with the coverage of the estimators 

summarizeCoverage <- function(result, cilNames, ciuNames, 
                              truthName = "truth", ...){
	coverage <- rep(NA, length(cilNames))
	truth <- result[,truthName]	
	for(i in 1:length(cilNames)){
		cil <- result[,cilNames[i]]
		ciu <- result[,ciuNames[i]]
		coverage[i] <- mean(cil < truth & ciu > truth, na.rm= TRUE)
	}
	names(coverage) <- cilNames
	coverage
}


#' plotBias
#' 
#' Function to plot the median percent bias for a given set of estimators
#' stratified by a given set of covariates
#' @param result The result data frame
#' @param estNames The names of the estimators to plot
#' @param strat The name of the variable on which to stratify
#' @param truthName The column name containing the true ATE
#' @param pointOptions A list of arguments to be passed to points
#' @param legendOptions A list of arguments passed to legend. pointOptions are added
#' automatically
#' @param ylim y-axis limits for the plot. If null, looks for range and adds 5% to it
#' @param estLabels Vector of names to label estimators in legend (default is to use estNames)
#' @param summary A list with two entries named "fn" and "options". This is the summary
#' function used to summarize percent difference. Defaults to median (i.e., \code{function = quantile},
#' \code{options = list(probs = 0.5)}). Assumes that the function will play nicely with the unnamed
#' argument passed in 
#' @param pctTruth Report error as absolute percent of true ATE? If \code{FALSE}, error is reported as 
#' absolute units of information (i.e., inverse of variance of EIF)
#' @param ... Additional arguments passed to plot

plotBias <- function(result, estNames, strat, truthName = "truth",
                     pointOptions = list(pch = 1:length(estNames)),
                     estLabels = estNames, 
                     legendOptions = list(x = "topleft", legend = estLabels),
                     ylim = NULL,
                     pctTruth = TRUE,
                     summary = list(fn = "quantile", options = list(probs=0.5)),
                     ...){
	nEst <- length(estNames)
	if(is.numeric(result[1,strat])){
		strata <- unique(sort(result[,strat]))		
	}else{
		strata <- unique(result[,strat])
	}
	if(pctTruth){
		biasList <- lapply(split(strata, 1:length(strata)), function(s){
			apply(result[result[,strat]==s,c(estNames)], 2, function(x){
				do.call(summary$fn, 
				        args = c(list(abs(x/result[result[,strat]==s,truthName]-1)),
				                 summary$options))
			})
		})
	}else{
		biasList <- lapply(split(strata, 1:length(strata)), function(s){
			apply(result[result[,strat]==s,c(estNames)], 2, function(x){
				do.call(summary$fn, 
				        args = c(list(x=abs(sqrt(result$n[result[,strat]==s])*(x-result[result[,strat]==s,truthName])/sqrt(result$varEffIC[result[,strat]==s]))),
				                 summary$options))
			})
		})
	}

	# get some limit information
	thisRange <- range(unlist(biasList))
	# add 5% to range to get y-limits 
	if(is.null(ylim)){
		ylim <- thisRange + c(-0.05,0.05)*thisRange	
	}
	# xlimits 
	xlim <- c(1,length(strata))
	# x-axis labels
	xaxt_lab <- strata
	
	# set up empty plot
	plot(0,0,pch=NULL,xlim=xlim,ylim=ylim,
	     xaxt = "n", 
	     ylab = ifelse(pctTruth,
	                   expression(paste("|",(hat(psi) - psi[0])/psi[0],"|")),
	                   expression(paste("|",n^{1/2},(hat(psi) - psi[0]),"| / Var")^{1/2}*"(EIF)")
	                   ),bty="n",
	     ...)
	# add x-axis
	axis(side = 1, at = 1:length(strata), label=strata)

	# add results in a loop
	for(i in 1:length(strata)){
		do.call(points, c(list(x=rep(i,nEst),y=biasList[[i]]),
		                  pointOptions))
	}
	# add a legend
	do.call(legend, c(legendOptions, pointOptions))
}


#' plotCoverage
#' 
#' Function to plot the coverage for a given set of estimators
#' stratified by a given set of covariates
#' @param result The result data frame
#' @param cilNames Column name(s) of results data frame that are lower confidence bounds
#' @param ciuNames Column name(s) of results data frame that are upper confidence bounds
#' @param strat The name of the variable on which to stratify
#' @param truthName The column name containing the true ATE
#' @param pointOptions A list of arguments to be passed to points
#' @param legendOptions A list of arguments passed to legend. pointOptions are added
#' automatically
#' @param ylim y-axis limits for the plot. If null, looks for range and adds 5% to it
#' @param estLabels Vector of names to label estimators in legend (default is to use estNames)
#' @param ... Additional arguments passed to plot

plotCoverage <- function(result, cilNames, ciuNames, strat, truthName = "truth",
                     pointOptions = list(pch = 1:length(cilNames)),
                     estLabels = cilNames, 
                     legendOptions = list(x = "topleft", legend = estLabels),
                     ylim = NULL,
                     ...){
	nEst <- length(cilNames)
	if(is.numeric(result[1,strat])){
		strata <- unique(sort(result[,strat]))		
	}else{
		strata <- unique(result[,strat])
	}
	covList <- lapply(split(strata, 1:length(strata)), function(s){
		coverage <- rep(NA, length(cilNames))
		truth <- result[result[,strat]==s,truthName]
		for(i in 1:length(cilNames)){
			cil <- result[result[,strat]==s,cilNames[i]]
			ciu <- result[result[,strat]==s,ciuNames[i]]
			coverage[i] <- mean(cil < truth & ciu > truth, na.rm = TRUE)
		}
		names(coverage) <- cilNames
		coverage
	})

	# get some limit information
	thisRange <- range(unlist(covList))
	# add 5% to range to get lower y-limits, upper y-limit is 1
	if(is.null(ylim)){
		ylim <- c(thisRange[1]-0.05, 1)
	}
	# xlimits 
	xlim <- c(1,length(strata))
	# x-axis labels
	xaxt_lab <- strata
	
	# set up empty plot
	plot(0,0,pch=NULL,xlim=xlim,ylim=ylim,
	     xaxt = "n", 
	     ylab = "Coverage of nominal 95% CI",bty="n",
	     ...)
	# add x-axis
	axis(side = 1, at = 1:length(strata), label=strata)

	# add results in a loop
	for(i in 1:length(strata)){
		do.call(points, c(list(x=rep(i,nEst),y=covList[[i]]),
		                  pointOptions))
	}
	# add a legend
	do.call(legend, c(legendOptions, pointOptions))
	# add a dashed line at 0.95
	abline(h = 0.95, lty = 3)
}

#' findBestSims
#' 
#' Locate the simulations where a particular estimator is performing the best
#' 
#' @param result The results data frame
#' @param estName The estimator that you want to find good simulations for
#' @param compEst Define best by comparing to other estimators or by being better
#' than a certain threshold in absolute error. 
#' @param threshBest The threshold that defines a best simulation (in pct of truth or
#' Z-score depend on value of \code{pctTruth})
#' @param compEstNames The estimators that you want to compare the chosen estimator
#' to in order to define good (e.g., all other estimators)
#' @param nSims The maximum number of simulations to return 
#' @param pctTruth Base on bias as percent of truth or in units of information
#' 

findBestSims <- function(result, estName, compEst = TRUE, compEstNames, nSims = 100,
                         pctTruth = TRUE, threshBest = 0.05, 
                         truthName = "truth", ...){
	# find |est - truth / truth| for all estimators
	if(pctTruth){
		allBias <- apply(result[,c(estName,compEstNames)], 2, function(x){
			abs(x/result[,truthName]-1)
		})
	}else{
		result <- result[!is.na(result$varEffIC),]
		allBias <- apply(result[,c(estName,compEstNames)], 2, function(x){
			abs(sqrt(result$n)*(x-result[,truthName])/sqrt(result$varEffIC))
		})
	}
	
	# if defining best by comparing to other estimators
	if(compEst){
		# get rank for each estimator
		allRanks <- apply(allBias, 1, rank, na.last = NA)
		if(is.list(allRanks)){
			allRanks <- Reduce(rbind, allRanks)
		}
		# get all simulations where estName wins
		bestSims <- which(allRanks[1,] == 1)
		# stop if it never wins
		if(is.null(bestSims)){
			stop("estName never was best estimator!")
		}
		# restrict to only these results
		bestResult <- allBias[bestSims,]
		# compute difference between estName and next closest est
		nCompEst <- length(compEstNames)
		nextClosest <- apply(bestResult, 1, function(x){
			nextBest <- which.min(x[1]/x[2:nCompEst]) + 1
			x[nextBest]
		})
		# order of results from biggest to smallest
		tmpInd <- order(-nextClosest)
		# output
		out <- list()
		# number of times it was the best
		out$pctBest <- length(bestSims)/length(result[,1])

		# the best nSim rows of result
		if(length(tmpInd) > nSims){
			out$bestResults <- result[bestSims[tmpInd],][1:nSims,]
		}else{
			out$bestResults <- result[bestSims[tmpInd],]
		}
		return(out)
	}else{
		# if defining best by being smaller than threshBest
		tmpInd <- which(allBias[,1] < threshBest)
		# output
		out <- list()
		# number of times it was best
		out$pctBest <- length(tmpInd)/length(result[,1])
		out$bestResults <- result[tmpInd,]
		return(out)
	}
}


#' findWorstSims
#' 
#' Locate the simulations where a particular estimator is performing the worst
#' 
#' @param result The results data frame
#' @param estName The estimator that you want to find bad simulations for
#' @param compEstNames The estimators that you want to compare the chosen estimator
#' to in order to define bad (e.g., all other estimators)
#' @param nSims The maximum number of simulations to return 
#' @param pctTruth Base on bias as percent of truth or in units of information
#' @param compEst Define best by comparing to other estimators or by being better
#' than a certain threshold in absolute error. 
#' @param threshWorst The threshold that defines a best simulation (in pct of truth or
#' Z-score depend on value of \code{pctTruth})


findWorstSims <- function(result, estName, compEstNames, nSims = 1000,
                          pctTruth = TRUE, compEst = TRUE, threshWorst = 4,
                         truthName = "truth", ...){
	if(pctTruth){
		allBias <- apply(result[,c(estName,compEstNames)], 2, function(x){
			abs(x/result[,truthName]-1)
		})
	}else{
		result <- result[!is.na(result$varEffIC),]
		allBias <- apply(result[,c(estName,compEstNames)], 2, function(x){
			abs(sqrt(result$n)*(x-result[,truthName])/sqrt(result$varEffIC))
		})
	}
	if(compEst){
		# get rank for each estimator
		allRanks <- apply(allBias, 1, rank)
		# get all simulations where estName wins
		worstSims <- which(allRanks[1,] == (length(compEstNames)+1))
		# stop if it never wins
		if(is.null(worstSims)){
			stop("estName never was worst estimator!")
		}
		# restrict to only these results
		worstResult <- allBias[worstSims,]
		# compute difference between estName and next closest est
		nCompEst <- length(compEstNames)
		nextClosest <- apply(worstResult, 1, function(x){
			nextWorst <- which.min(x[1]/x[2:nCompEst]) + 1
			x[nextWorst]
		})
		# order of results from biggest to smallest
		tmpInd <- order(-nextClosest)
		# output
		out <- list()
		# number of times it was the best
		out$pctWorst <- length(worstSims)/length(result[,1])
		# the best nSim rows of result
		if(length(tmpInd) > nSims){
			out$worstResults <- result[worstSims[tmpInd],][1:nSims,]
		}else{
			out$worstResults <- result[worstSims[tmpInd],]
		}
		return(out)
	}else{
		# if defining best by being larger than threshWorst
		tmpInd <- which(allBias[,1] > threshWorst)
		# output
		out <- list()
		# number of times it was best
		out$pctWorst <- length(tmpInd)/length(result[,1])
		out$worstResults <- result[tmpInd,]
		return(out)
	}
}


#' plotSamplingDist
#' 
#' Plot the centered, scaled (by sqrt(var(EIF)/n)) sampling distribution 
#' of a given set of estimators. 
#' @param result Result data frame
#' @param estNames Column name(s) of results data frame that are estimators
#' @param truthName Column name of results data frame that is the true ATE
#' @param incAsymDist Whether to also include the asymptotic distribution
#' @param legendOptions A list of arguments passed to legend.
#' @param densityOptions A list of options passed to \code{density}
#' @param ylim y-axis limits for the plot. If null, looks for range and adds 5% to it
#' @param estLabels Vector of names to label estimators in legend (default is to use estNames)
#' @param ... Other arguments passed to plot

plotSamplingDist <- function(result, estNames,
                             estLabels = estNames,
                             legendOptions = list(x = "topleft", legend = c(estLabels,"N(0,1)")),
                             incAsymDist = TRUE, 
                             densityOptions = list(n=2^12),
                             truthName = "truth",
                             ylim = NULL,
                             xlim = NULL,
                             ...){
	nEst <- length(estNames)
	# find z for each estimator
	zList <- vector(mode = "list", length = length(nEst))
	for(i in 1:nEst){
		tmp <- sqrt(result$n)*(result[,estNames[i]]-result$truth)/sqrt(result$varEffIC)
		zList[[i]] <- tmp[!is.na(tmp)]
	}
	# call density
	densList <- lapply(zList, function(x){
		do.call(density, c(list(x=x, bw=1.06*sd(x)*length(x)^(-1/5)),densityOptions))
	})

	# make empty plot
	if(is.null(xlim)){
		xlim <- range(unlist(lapply(densList, function(x){x$x})))
	}
	if(is.null(ylim)){
		ylim <- range(unlist(lapply(densList, function(x){x$y}))) 
	}
	plot(0,0,pch="",xlim=xlim,ylim=ylim,bty="n",ylab = "sqrt(n)*(est-truth)/sd(EIF)")

	# add densities in loop
	for(i in 1:nEst){
		lines(densList[[i]], lwd = 2, col = i)
		# tmp <- zList[[i]]
		# hist(tmp[abs(tmp)<5],freq = FALSE, add = TRUE, col = i,breaks=20)
	}

	# add reference if it's there
	if(incAsymDist){
		trueCol <- rgb(1,0,0,0.25)
		xSeq <- seq(xlim[1],xlim[2],length=length(densList[[1]]$x))
		lines(y=dnorm(xSeq),x=xSeq,
		      col = trueCol,lty = 3,lwd=2)
	}

	do.call(legend, c(legendOptions, list(lty = c(rep(1,nEst),3),lwd = 2, col = c(1:nEst,trueCol))))

}

#' plotDiff
#' 
#' Plot the difference between the scaled version of two estimators. The estimator 
#' names are entered in \code{estNames} and the difference between absolute value of
#' Z-scores for estNames[1] - estNames[2] is plotted. 
#' 
#' @param result Result data frame
#' @param estNames Column names of results data frame that are the two estimators to be compared
#' @param truthName Column name of results data frame that is the true ATE
#' @param ylim y-axis limits for the plot. If null, looks for range and adds 5% to it
#' @param estLabels Vector of names to label estimators in legend (default is to use estNames)
#' @param ... Other arguments passed to plot

plotDiff <- function(result, estNames,
                             estLabels = estNames,
                             truthName = "truth",
                             ylim = NULL,
                             xlim = NULL,
                             ...){
	nEst <- length(estNames) # should be 2
	if(nEst != 2) stop("length of estNames should be 2")
	# find z for each estimator
	zList <- vector(mode = "list", length = length(nEst))
	for(i in 1:nEst){
		# returning absolute value of standardized estimator
		tmp <- abs(sqrt(result$n)*(result[,estNames[i]]-result[,truthName])/sqrt(result$varEffIC))
		
		# returning percent error
		# tmp <- abs(result[,estNames[i]]/result[,truthName]-1)

		# get rid of NAs
		zList[[i]] <- tmp[!is.na(tmp)]
	}
	diffEst <- Reduce("-",zList)

	# quantile to get range of points
	xlim <- quantile(diffEst, probs = c(0.01,0.99))

	# compute P(diffEst < x) for sequence of x's
	xSeq <- seq(xlim[1],xlim[2], length=1000)
	pDiff <- rep(NA,1000)
	for(i in 1:1000){
		pDiff[i] <- sum(diffEst < xSeq[i])/length(diffEst)
	}
	# make empty plot
	if(is.null(ylim)){
		ylim <- c(0,1)
	}
	plot(pDiff ~ xSeq,lwd=2,pch="",xlim=xlim,ylim=ylim,bty="n",
	     xlab = "x",type = "l",...)

	# add plot to show where CDF cross 0
	cP <- sum(diffEst < 0)/length(diffEst)
	segments(x0=0,y0=0,y1=cP,lty=3)
	segments(x0=par()$usr[1],x1=0,y0=cP,lty=3)
	text(x = par()$usr[1], y = cP, adj=c(0,0),ifelse(round(cP,2)==0,"<0.01",round(cP,2)),xpd=TRUE)
	# add plot to show where CDF crosses -1
	cP <- sum(diffEst < -1)/length(diffEst)
	segments(x0=-1,y0=0,y1=cP,lty=3)
	segments(x0=par()$usr[1],x1=-1,y0=cP,lty=3)
	text(x = par()$usr[1], y = cP, adj=c(0,0), ifelse(round(cP,2)==0,"<0.01",round(cP,2)),xpd=TRUE)
	# add plot to show where CDF crosses 1
	cP <- sum(diffEst < 1)/length(diffEst)
	segments(x0=1,y0=0,y1=cP,lty=3)
	segments(x0=par()$usr[1],x1=1,y0=cP,lty=3)
	text(x = par()$usr[1], y = cP, adj=c(0,0),ifelse(round(cP,2)==0,"<0.01",round(cP,2)),xpd=TRUE)

	
	# add text on y-axis

	# add a rug plot
	rug(diffEst,col="gray75")
	# add density
	dens <- density(x=diffEst, bw=1.06*sd(diffEst)*length(diffEst)^(-1/5),n=2^12)
	densScale <- (dens$y - min(dens$y))/diff(range(dens$y))
	lines(y=densScale, x=dens$x, col="gray75")
	axis(side = 4, col.axis="gray75","p(abs(Z-HAL) - abs(Z-GLM) < x)",at=seq(0,1,0.2),
	     labels = round(diff(range(dens$y))*seq(0,1,0.2),2),col.lab="gray75",col="gray75")
}



# # names of estimators/confidence intervals
# colNames_allOut <- colnames(allOut)

# # names of all estimators
# # cilNames <- colNames_allOut[grep("\\.cil", colNames_allOut)]
# # ciuNames <- colNames_allOut[grep("\\.ciu", colNames_allOut)]
# # compEstNames <- colNames_allOut[grep("\\.est", colNames_allOut)]

# # names of selected estimators
# estNames <- c("SL.hal.est","slFull.est","slDrop.est","SL.step.interaction.est","SL.glm.est")
# ciuNames <- c("SL.hal.ciuOrc","slFull.ciuOrc","slDrop.ciuOrc","SL.step.interaction.ciuOrc","SL.glm.ciuOrc")
# cilNames <- c("SL.hal.cilOrc","slFull.cilOrc","slDrop.cilOrc","SL.step.interaction.cilOrc","SL.glm.cilOrc")
# # bias
# summarizeBias(result = allOut, estNames = estNames)

# # coverage
# summarizeCoverage(result = allOut, cilNames = cilNames, ciuNames = ciuNames)

# # by sample size
# summarizeBias(result = allOut[allOut$n==100,], estNames = estNames)
# summarizeBias(result = allOut[allOut$n==500,], estNames = estNames)
# summarizeBias(result = allOut[allOut$n==1000,], estNames = estNames)
# summarizeBias(result = allOut[allOut$n==2000,], estNames = estNames)

# summarizeCoverage(result = allOut[allOut$n==100,], cilNames = cilNames, ciuNames = ciuNames)
# summarizeCoverage(result = allOut[allOut$n==500,], cilNames = cilNames, ciuNames = ciuNames)
# summarizeCoverage(result = allOut[allOut$n==1000,], cilNames = cilNames, ciuNames = ciuNames)
# summarizeCoverage(result = allOut[allOut$n==2000,], cilNames = cilNames, ciuNames = ciuNames)

# # by dimension
# summarizeBias(result = allOut[allOut$D==1,], estNames = estNames)
# summarizeBias(result = allOut[allOut$D==2,], estNames = estNames)
# summarizeBias(result = allOut[allOut$D==3,], estNames = estNames)
# summarizeBias(result = allOut[allOut$D==4,], estNames = estNames)
# summarizeBias(result = allOut[allOut$D==5,], estNames = estNames)
# summarizeBias(result = allOut[allOut$D==6,], estNames = estNames)
# summarizeBias(result = allOut[allOut$D==7,], estNames = estNames)
# summarizeBias(result = allOut[allOut$D==8,], estNames = estNames)



# # debugging code for function
# result <- allOut
# truthName <- "truth"


# # find DGMs with actual confounding
# indConf <- ((allOut$obsATE - allOut$truth)/allOut$truth) > 2

# # summary for bias
# y <- log(abs((allOut$SL.hal.est - allOut$truth)/allOut$truth))

# grbg <- step(glm(y ~ n + D + minObsG0 + mainTermsG0 + bivTermsG0 + triTermsG0 + mainTermsQ0 + bivTermsQ0 +
#     triTermsQ0 + r2 + nCorr1 + nCorr2 + nCorr3 + nCorr4, data = allOut))
# summary(grbg)

# # summary for coverage
# y <- as.numeric(allOut$SL.hal.cil < allOut$truth & allOut$SL.hal.ciu > allOut$truth)
# grbg <- step(glm(y ~ n + D + minObsG0 + mainTermsG0 + bivTermsG0 + triTermsG0 + mainTermsQ0 + bivTermsQ0 +
#     triTermsQ0 + r2 + nCorr4, data = allOut))
# summary(grbg)



# # where do full and drop SL differ
# grbg <- (allOut$slFull.est - allOut$slDrop.est)/allOut$slFull.est


