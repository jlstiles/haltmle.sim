#' normalErr
#' 
#' Draw an error from a normal distribution with particular variance
#' @export
normalErr <- function(n, sd, ...){
	return(rnorm(n,0,sd))
}
#' normalErrParm
#' 
#' Draw a random standard deviation for \code{normalErr}
#' @export
normalErrParm <- function(sdLower = 0.5, sdUpper = 7){
	return(list(sd = runif(1, sdLower, sdUpper)))
}

#' uniformErr
#' 
#' Draw an error from a uniform distribution with particular range
#' @export
uniformErr <- function(n, range, ...){
	return(runif(n, -range/2, range/2))
}

#' uniformErrParm
#' 
#' Generate parameters for \code{uniformErr}
#' @export
uniformErrParm <- function(rangeLower = 1, rangeUpper = 25){
	return(list( range = runif(1, rangeLower, rangeUpper)))
}


#' gammaErr
#' 
#' Draw an error from a shifted gamma distribution with particular range
#' @export
gammaErr <- function(n,a,b,...){
	tmp <- rgamma(n,a,b)
	tmp <- tmp - mean(tmp)
	return(tmp)
}

#' gammaErrParm
#' Generate parameters for \code{gammaErr}
#' @export
gammaErrParm <- function(aLower = 0.1, aUpper = 7.5,
                      bLower = 0.1, bUpper = 7.5){
	return(list(a = runif(1, aLower, aUpper),
	       b = runif(1, bLower, bUpper)))
}

#' normalErrW 
#' 
#' Draw an error from a normal distribution with a particular variance that depends on one W
#' @export
normalErrW <- function(n, sd, W){
	err <- rep(0, length(W[,1]))
	# binary W
	if(all(W[,1] %in% c(0,1))){
		err[W[,1] == 0] <- rnorm(sum(W[,1]==0), 0, sd)
		err[W[,1] == 1] <- rnorm(sum(W[,1]==1), 0, 2*sd)
	}else{
		err <- rnorm(n, 0, plogis(W[,1])*sd)
	}
	return(err)
}

#' normalErrWParm 
#' 
#' Generate parameters for \code{normalErrUniW}
#' @export
normalErrWParm <- function(sdLower = 0.5, sdUpper = 7){
	return(list(sd = runif(1, sdLower, sdUpper)))
}


#' uniformErrW
#' 
#' Draw an error from a uniform distribution with range determined by W
#' @export
uniformErrW <- function(n, range, W){
	err <- rep(0, n)
	# binary W
	if(all(W[,1] %in% c(0,1))){
		err[W[,1] == 0] <- runif(sum(W[,1]==0), -range/2, range/2)
		err[W[,1] == 1] <- runif(sum(W[,1]==1), -range, range)
	}else{
		err <- runif(n, -plogis(W[,1])*range, plogis(W[,1])*range)
	}
	return(err)
}


#' uniformErrWParm
#' 
#' Generate parameters for \code{uniformErr}
#' @export
uniformErrWParm <- function(rangeLower = 1, rangeUpper = 25){
	return(list(range = runif(1, rangeLower, rangeUpper)))
}

