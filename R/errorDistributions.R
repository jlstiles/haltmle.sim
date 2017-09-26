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
normalErrParm <- function(...,sdLower = 0.5, sdUpper = 7){
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
uniformErrParm <- function(...,rangeLower = 1, rangeUpper = 25){
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
gammaErrParm <- function(...,aLower = 0.1, aUpper = 7.5,
                      bLower = 0.1, bUpper = 7.5){
	return(list(a = runif(1, aLower, aUpper),
	       b = runif(1, bLower, bUpper)))
}

#' normalErrAW 
#' 
#' Draw an error from a normal distribution with a particular variance that depends on one W
#' @export
normalErrAW <- function(n, sd, co, AW){
	err <- rep(0, n)
	# binary W
	for(i in co){
		if(all(AW[,i] %in% c(0,1))){
			err[AW[,i] == 0] <- err[AW[,i] == 0] + rnorm(sum(AW[,i]==0), 0, sd)
			err[AW[,i] == 1] <- err[AW[,i] == 1] + rnorm(sum(AW[,i]==1), 0, 2*sd)
		}else{
			err <- err + rnorm(n, 0, plogis(AW[,i])*sd)
		}
	}	
	return(err)
}

#' normalErrAWParm 
#' 
#' Generate parameters for \code{normalErrUniW}
#' @export
normalErrAWParm <- function(AW,sdLower = 0.5, sdUpper = 7,
                            nDepLower = 0.5, nDepUpper = 3.5){
	nDep = round(runif(1, nDepLower, nDepUpper))
	# randomly sample column from AW
	co <- sample(1:ncol(AW), nDep, min(nDep, ncol(AW)))

	return(list(sd = runif(1, sdLower, sdUpper),
	            co = co))
}

#' uniformErrAW
#' 
#' Draw an error from a uniform distribution with range determined by W
#' @export
uniformErrAW <- function(n, range, co, AW){
	err <- rep(0, n)
	# binary W
	for(i in co){
		if(all(AW[,i] %in% c(0,1))){
			err[AW[,i] == 0] <- err[AW[,i] == 0] + runif(sum(AW[,i]==0), -range/2, range/2)
			err[AW[,i] == 1] <- err[AW[,i] == 1] + runif(sum(AW[,i]==1), -range, range)
		}else{
			err <- err + runif(n, -plogis(AW[,i])*range, plogis(AW[,i])*range)
		}
	}
	return(err)
}


#' uniformErrAWParm
#' 
#' Generate parameters for \code{uniformErr}
#' @export
uniformErrAWParm <- function(AW,rangeLower = 1, rangeUpper = 25,
                             nDepLower = 0.5, nDepUpper = 3.5){
	nDep <- round(runif(1, nDepLower, nDepUpper))
	# randomly sample column from AW
	co <- sample(1:ncol(AW), min(nDep, ncol(AW)))

	return(list(range = runif(1, rangeLower, rangeUpper),
	            co = co))
}

