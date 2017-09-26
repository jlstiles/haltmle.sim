#' plot.makeRandomData
#' 
#' Plot the results of makeRandomData
#' 
#' @param object An object of class \code{"makeRandomData"}
#' @param g0 Make plots of propensity scores
#' @param Q0 Make plots of outcome regressions
#' @param ask Make plots one-by-one (if TRUE) or put all on one page using \code{layout}
#' @param nPoints The number of points to plot for each line showing 
#' propensity scores and outcome regressions
#' @param quantProbs For variables that have interactions, the PS and OR are plotted
#' at the median level of all variables not in the interaction and at these quantiles of
#' any non-binary variables that are in the interaction. Defaults to the first and third quartile.
#' Also by default if there is an interaction with a binary variable, rather than plotting curves
#' at the quantile levels, the function plots at the unique values of the binary variable.
#' @param addMain Add a somewhat descriptive main title to the plot?
#' @param layOutNum The matrix numbers fed to \code{layout}
#' @param legLoc Passed to the \code{x} argument of \code{legend} to place legend (for all plots)
#' @export

plot.makeRandomData <- function(object, g0 = TRUE, Q0 = TRUE, ask = FALSE, addMain = TRUE,
                                nPoints = 1000, quantProbs = c(0.25, 0.75), 
                                nrow = 3, ncol = 2,
                                layOutNum = 1:(nrow*ncol),
                                legLoc = "topleft"){
	n <- length(object$W[,1])
	if(ask){
		par(ask = TRUE)
	}else{
		layout(matrix(layOutNum, nrow=nrow, ncol = ncol, byrow = TRUE))
	}

	if(!ask) par(mgp=c(2,0.5,0), mar = c(3.1,3.1,0.5,0.5))
	for(m in 1:ncol(object$W)){
		bigRemake <- remakeRandomData(n=1e5,object = object)
		# plot a histogram of each variable
		if(addMain){
			main <- bquote(paste(W[.(m)] %~% .(object$distW[[m]]$fn), "(", .(paste(names(object$distW[[m]]$parm), round(unlist(object$distW[[m]]$parm),2), sep= "=", collapse=",")),")"))
		}else{
			main <- ""
		}
		hist(bigRemake$W[,m], bty="n", xlab = bquote(W[.(m)]),
		     main = main,freq = FALSE)
	}

	#----------------------------------------------
	# plot the propensity score as a function of each variable
	#----------------------------------------------
	# find what variables have interactions
	bivIntVar <- lapply(object$fnG0$biv, function(x){x$whichColsW})
	triIntVar <- lapply(object$fnG0$tri, function(x){x$whichColsW})
	quadIntVar <- lapply(object$fnG0$quad, function(x){x$whichColsW})

	mainTermsG0 <- length(object$fnG0$uni)
	for(m in 1:ncol(object$W)){
		# see if there are higher order interactions with W[,m]
		intM <- (m %in% unlist(bivIntVar) | m %in% unlist(triIntVar) | m %in% unlist(quadIntVar))

		if(addMain){
			if(m <= length(object$fnG0$uni)){
				main <- bquote(paste(logit(g[0]) %~% .(object$fnG0$uni[[m]]$fn), "(", 
			                         .(paste(names(object$fnG0$uni[[m]]$parm), 
			                                 lapply(object$fnG0$uni[[m]]$parm, function(x){ paste(round(x,2),collapse=",") }),
			                                 sep= "=", collapse=",")),")"
			     					, .(ifelse(intM,"+ int.",""))))
			}else{
				if(intM){
					main <- bquote(paste(logit(g[0]) %~% "int."))
				}else{
					main <- bquote(paste(logit(g[0]) %~% "1"))
				}
			}
		}else{
			main <- ""
		}
		uniqW <- unique(object$W[,m])
		nUniq <- length(uniqW)
		minW <- min(object$W[,m])
		maxW <- max(object$W[,m])
		
		# if no interaction
		if(!intM){
			# create data frame with median values for every covariate
			if(nUniq < n){
				medianDat <- matrix(rep(apply(object$W,2,median),nUniq), nrow = nUniq, byrow = TRUE)
				medianDat[,m] <- uniqW
				numPoints <- nUniq
				remakeDat <- remakeRandomData(n = numPoints, object = object, setW = medianDat)
				plot(remakeDat$g0 ~ remakeDat$W[,m,drop=FALSE], xlab = bquote(W[.(m)]), ylab = paste0("PS"), 
								     bty="n",lwd = 2, main = main)
			}else{
				medianDat <- matrix(rep(apply(object$W,2,median),nPoints), nrow = nPoints, byrow = TRUE)
				medianDat[,m] <- seq(minW, max(object$W[,m]),length=nPoints)
				numPoints <- nPoints
				remakeDat <- remakeRandomData(n = numPoints, object = object, setW = medianDat)
				plot(remakeDat$g0 ~ remakeDat$W[,m,drop=FALSE], xlab = bquote(W[.(m)]), ylab = paste0("PS"), 
				     bty="n", type="l",lwd = 2, main = main)
				# rug(object$W[,m], col = "gray90")
			}
		}else{ # if interaction
			# figure out what variables this one interacts with
			otherVars <- lapply(c(bivIntVar, triIntVar, quadIntVar), function(x){
				if(m %in% x){
					return(x[x!=m])
				}else{
					return(NULL)
				}
			})
			# unique values of those other variables
			uniqOtherVars <- unique(unlist(otherVars))
			nOtherVars <- length(uniqOtherVars)
			# all variables at 25th and 75th percentile (ideally or at 0/1, depending)
			grbg <- vector(mode = "list", length = nOtherVars)
			for(i in 1:nOtherVars){
				# check if dichotomous
				if(length(unique(object$W[,uniqOtherVars[i]])) == 2){
					grbg[[i]] <- unique(object$W[,uniqOtherVars[i]])
				}else{
					grbg[[i]] <- quantile(object$W[,uniqOtherVars[i]], quantProbs)
				}
			}
			# expand grid to get all combinations
			valueMat <- expand.grid(grbg)

			# make plot if this W variable is binary
			if(nUniq < n){
				medianDat <- matrix(rep(apply(object$W,2,median),nUniq), nrow = nUniq, byrow = TRUE)
				medianDat[,m] <- uniqW
				# empty legend labels
				legVal <- NULL
				# loop over different values to replace values with values of variables
				# that we want
				for(i in 1:nrow(valueMat)){
					# loop over columns of valueMat to add points to plot for each
					# value combination
					for(j in 1:ncol(valueMat)){
						medianDat[,uniqOtherVars[j]] <- valueMat[i,j]
					}
					# remake random data
					remakeDat <- remakeRandomData(n = nUniq, object = object, setW = medianDat)
					# make plot
					if(i==1){
						plot(remakeDat$g0 ~ remakeDat$W[,m,drop=FALSE], xlab = bquote(W[.(m)]), 
						     ylab = paste0("PS"), 
					     	 bty="n", pch = i, lwd = 2, main = main, ylim = c(0,1))
					}else{
						points(remakeDat$g0 ~ remakeDat$W[,m,drop=FALSE], pch = i, lwd = 2)
					}
					# for legend labels
					tmpLegVal <- NULL
					for(j in 1:ncol(valueMat)){
						tmpLegVal <- c(tmpLegVal, paste0("{W[",uniqOtherVars[j],"]==",round(valueMat[i,j],2),"}"))
					}
					legVal <- c(legVal, paste(tmpLegVal,collapse="*','*"))
				} # end loop over valueMat
				# add a legend
				legend(x=legLoc,pch=1:nrow(valueMat),
				       legend=as.expression(sapply(legVal,function(x){bquote(.(parse(text=x)))},simplify=TRUE,USE.NAMES=FALSE)))
			# make plot if this W variable is categorical or continuous
			}else{
				# make initial data set setting value to median for all variables
				medianDat <- matrix(rep(apply(object$W,2,median),nPoints), nrow = nPoints, byrow = TRUE)
				# replace values for W[,m] with a sequence from it's smallest to largest
				# value of length nPoints
				medianDat[,m] <- seq(minW, max(object$W[,m]),length=nPoints)
				# empty legend labels
				legVal <- NULL
				# loop over different values to replace values with values of variables
				# that we want
				for(i in 1:nrow(valueMat)){
					# browser()
					# loop over columns of valueMat to add points to plot for each
					# value combination
					for(j in 1:ncol(valueMat)){
						medianDat[,uniqOtherVars[j]] <- valueMat[i,j]
					}
					# remake random data
					remakeDat <- remakeRandomData(n = nPoints, object = object, setW = medianDat)
					# make plot
					if(i==1){
						plot(remakeDat$g0 ~ remakeDat$W[,m,drop=FALSE], xlab = bquote(W[.(m)]), 
						     ylab = paste0("PS"), type="l",
					     	 bty="n", lty = i, lwd = 2, main = main, ylim = c(0,1))
					}else{
						lines(remakeDat$g0 ~ remakeDat$W[,m,drop=FALSE], lty = i, lwd = 2)
					}
					# for legend labels
					tmpLegVal <- NULL
					for(j in 1:ncol(valueMat)){
						tmpLegVal <- c(tmpLegVal, paste0("{W[",uniqOtherVars[j],"]==",round(valueMat[i,j],2),"}"))
					}
					legVal <- c(legVal, paste(tmpLegVal,collapse="*','*"))
				} # end loop over valueMat
				# add a legend
				legend(x=legLoc,lty=1:nrow(valueMat),
				       legend=as.expression(sapply(legVal,function(x){bquote(.(parse(text=x)))},simplify=TRUE,USE.NAMES=FALSE)))

			}
		} # end interaction code
	} # end loop over columns of W for propensity plotting

	#----------------------------------------------
	# plot the outcome regression as a function of each variable
	#----------------------------------------------
	# find what variables have interactions
	bivIntVar <- lapply(object$fnQ0$biv, function(x){x$whichColsAW})
	triIntVar <- lapply(object$fnQ0$tri, function(x){x$whichColsAW})
	quadIntVar <- lapply(object$fnQ0$quad, function(x){x$whichColsAW})

	mainTermsQ0 <- length(object$fnQ0$uni)
	# create AW data frame
	AW <- cbind(object$A,object$W)
	for(m in 0:ncol(object$W)){
		# browser()
		# see if there are higher order interactions with W[,m]
		intM <- ((m+1) %in% unlist(bivIntVar)) |  ((m+1) %in% unlist(triIntVar)) | ((m+1) %in% unlist(quadIntVar))
		if(addMain){
			if((m+1) <= length(object$fnQ0$uni)){
				main <- bquote(paste(Q[0] %~% .(object$fnQ0$uni[[m+1]]$fn), "(", 
			                         .(paste(names(object$fnQ0$uni[[m+1]]$parm), 
			                                 lapply(object$fnQ0$uni[[m+1]]$parm, function(x){ paste(round(x,2),collapse=",") }),
			                                 sep= "=", collapse=",")),")"
			     					, .(ifelse(intM," + int.",""))))
			}else{
				if(intM){
					main <- bquote(paste(Q[0] %~% "int."))
				}else{
					main <- bquote(paste(Q[0] %~% "1"))
				}
			}
		}else{
			main <- ""
		}
		uniqW <- unique(AW[,(m+1)])
		nUniq <- length(uniqW)
		minW <- min(AW[,(m+1)])
		maxW <- max(AW[,(m+1)])
		
		# if no interaction
		if(!intM){
			# create data frame with median values for every covariate
			if(nUniq < n){
				# this is where it will go if AW[,(m+1)] is A
				# browser()
				medianDat <- matrix(rep(apply(AW,2,median),nUniq), nrow = nUniq, byrow = TRUE)
				medianDat[,(m+1)] <- uniqW
				numPoints <- nUniq
				# remake the data setting A and W to median values
				remakeDat <- remakeRandomData(n = numPoints, object = object, setA = medianDat[,1],
				                              setW = medianDat[,2:ncol(medianDat)])
				if(m!=0){
					plot(remakeDat$Q0 ~ remakeDat$W[,m,drop=FALSE], xlab = bquote(W[.(m)]), ylab = paste0("OR"), 
									     bty="n",lwd = 2, main = main, ylim = range(object$Y))
				}else{
					plot(remakeDat$Q0 ~ remakeDat$A, xlab = "A", ylab = paste0("OR"), 
									     bty="n",lwd = 2, main = main, ylim = range(object$Y))
				}
			}else{
				# will never go here if AW[,(m+1)] is A so no need to modify for this case
				medianDat <- matrix(rep(apply(AW,2,median),nPoints), nrow = nPoints, byrow = TRUE)
				medianDat[,(m+1)] <- seq(minW, max(object$W[,m]),length=nPoints)
				numPoints <- nPoints
				remakeDat <- remakeRandomData(n = numPoints, object = object, setA = medianDat[,1],
				                              setW = medianDat[,2:ncol(medianDat)])
				plot(remakeDat$Q0 ~ remakeDat$W[,m,drop=FALSE], xlab = bquote(W[.(m)]), ylab = paste0("OR"), 
				     bty="n", type=ifelse(nUniq==n,"l","p"),pch=1,lwd = 2, main = main, ylim = range(object$Y))
				# rug(object$W[,m], col = "gray90")
			}
		}else{ # if interaction
			# browser()
			# figure out what variables this one interacts with
			otherVars <- lapply(c(bivIntVar, triIntVar, quadIntVar), function(x){
				if((m+1) %in% x){
					return(x[x!=(m+1)])
				}else{
					return(NULL)
				}
			})
			# unique values of those other variables
			uniqOtherVars <- unique(unlist(otherVars))
			nOtherVars <- length(uniqOtherVars)
			# all variables at 25th and 75th percentile (ideally or at 0/1, depending)
			grbg <- vector(mode = "list", length = nOtherVars)
			# put A and W into a data frame
			for(i in 1:nOtherVars){
				# check if dichotomous
				if(length(unique(AW[,uniqOtherVars[i]])) == 2){
					grbg[[i]] <- unique(AW[,uniqOtherVars[i]])
				}else{
					grbg[[i]] <- quantile(AW[,uniqOtherVars[i]], quantProbs)
				}
			}
			# expand grid to get all combinations
			valueMat <- expand.grid(grbg)

			# make plot if this W variable is binary (this is where we'll go
			# if m corresponds with the A column)
			if(nUniq < n){
				medianDat <- matrix(rep(apply(AW,2,median),nUniq), nrow = nUniq, byrow = TRUE)
				medianDat[,(m+1)] <- uniqW
				# empty legend labels
				legVal <- NULL
				# loop over different values to replace values with values of variables
				# that we want
				for(i in 1:nrow(valueMat)){
					# loop over columns of valueMat to add points to plot for each
					# value combination
					for(j in 1:ncol(valueMat)){
						medianDat[,uniqOtherVars[j]] <- valueMat[i,j]
					}
					# remake random data
					remakeDat <- remakeRandomData(n = nUniq, object = object, setA = medianDat[,1],
					                              setW = medianDat[,2:ncol(medianDat)])
					# make plot
					if(i==1){
						if(m != 0){
							plot(remakeDat$Q0 ~ remakeDat$W[,m,drop=FALSE], xlab = bquote(W[.(m)]), 
							     ylab = paste0("OR"), 
						     	 bty="n", pch = i, lwd = 2, main = main, ylim = range(object$Y))
						}else{
							plot(remakeDat$Q0 ~ remakeDat$A, xlab = "A", 
							     ylab = paste0("OR"), 
						     	 bty="n", pch = i, lwd = 2, main = main, ylim = range(object$Y))
						}
					}else{
						if(m != 0){
							points(remakeDat$Q0 ~ remakeDat$W[,m,drop=FALSE], pch = i, lwd = 2)							
						}else{
							points(remakeDat$Q0 ~ remakeDat$A, pch = i, lwd = 2)
						}
					}
					# for legend labels
					tmpLegVal <- NULL
					for(j in 1:ncol(valueMat)){
						# check if the interacting variable is a W
						if(uniqOtherVars[j] != 1){
							tmpLegVal <- c(tmpLegVal, paste0("{W[",uniqOtherVars[j]-1,"]==",round(valueMat[i,j],2),"}"))
						# if it's not then label it as A
						}else{
							tmpLegVal <- c(tmpLegVal, paste0("{A==",round(valueMat[i,j],2),"}"))
						}
					}
					legVal <- c(legVal, paste(tmpLegVal,collapse="*','*"))
				} # end loop over valueMat
				# add a legend
				# browser()
				legend(x=legLoc,pch=1:nrow(valueMat),
				       legend=as.expression(sapply(legVal,function(x){bquote(.(parse(text=x)))},simplify=TRUE,USE.NAMES=FALSE)))
			# make plot if this W variable is categorical or continuous
			# A will never come in here
			}else{
				# make initial data set setting value to median for all variables
				medianDat <- matrix(rep(apply(AW,2,median),nPoints), nrow = nPoints, byrow = TRUE)
				# replace values for W[,m] with a sequence from it's smallest to largest
				# value of length nPoints
				medianDat[,(m+1)] <- seq(minW, max(object$W[,m]),length=nPoints)
				# empty legend labels
				legVal <- NULL
				# loop over different values to replace values with values of variables
				# that we want
				for(i in 1:nrow(valueMat)){
					# browser()
					# loop over columns of valueMat to add points to plot for each
					# value combination
					for(j in 1:ncol(valueMat)){
						medianDat[,uniqOtherVars[j]] <- valueMat[i,j]
					}
					# remake random data
					remakeDat <- remakeRandomData(n = nPoints, object = object, setW = medianDat[,2:ncol(medianDat)],
					                              setA = medianDat[,1])
					# make plot
					if(i==1){
						plot(remakeDat$Q0 ~ remakeDat$W[,m,drop=FALSE], xlab = bquote(W[.(m)]), 
						     ylab = paste0("OR"), type=ifelse(nUniq==n,"l","p"),
					     	 bty="n", lty = i, pch = i, lwd = 2, main = main, ylim = range(object$Y))
					}else{
						if(nUniq==n){
							lines(remakeDat$Q0 ~ remakeDat$W[,m,drop=FALSE], lty = i, lwd = 2)
						}else{
							points(remakeDat$Q0 ~ remakeDat$W[,m,drop=FALSE], pch = i, lwd = 2)							
						}
					}
					# for legend labels
					tmpLegVal <- NULL
					for(j in 1:ncol(valueMat)){
						# check if the interacting variable is a W
						if(uniqOtherVars[j] != 1){
							tmpLegVal <- c(tmpLegVal, paste0("{W[",uniqOtherVars[j]-1,"]==",round(valueMat[i,j],2),"}"))
						# if it's not then label it as A
						}else{
							tmpLegVal <- c(tmpLegVal, paste0("{A==",round(valueMat[i,j],2),"}"))
						}					
					}
					legVal <- c(legVal, paste(tmpLegVal,collapse="*','*"))
				} # end loop over valueMat
				# add a legend
				if(nUniq==n){
					legend(x=legLoc,lty=1:nrow(valueMat),
				   	    legend=as.expression(sapply(legVal,function(x){bquote(.(parse(text=x)))},simplify=TRUE,USE.NAMES=FALSE)))
				}else{
					legend(x=legLoc,pch=1:nrow(valueMat),
				   	    legend=as.expression(sapply(legVal,function(x){bquote(.(parse(text=x)))},simplify=TRUE,USE.NAMES=FALSE)))
				}
			}
		} # end interaction code
	} # end loop over columns of W for outcome regression
}