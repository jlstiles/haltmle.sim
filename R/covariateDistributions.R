
#' uniformW
#' 
#' Draw from uniform(lower, upper) distribution
#' @export
uniformW <- function(n, lower, upper,...){
	return(runif(n, lower, upper))
}
#' uniformWParm
#' 
#' Generate parameters for \code{uniform}
#' @export

uniformWParm <- function(lowerLower = -10, lowerUpper = 0,
                        rangeLower = 0.5, rangeUpper = 10){
	lower <- runif(1, lowerLower, lowerUpper)
	upper <- lower + runif(1, rangeLower, rangeUpper)
	return(list(lower = lower, upper = upper))
}

#' bernoulliW
#' 
#' Draw from Bernoulli(p)
#' @export
bernoulliW <- function(n,p,...){
	return(rbinom(n,1,p))
}
#' bernoulliParm
#' 
#' Generate parameters for \code{bernoulli}
#' @export
bernoulliWParm <- function(pLower = 0.1, pUpper = 0.9){
	return(list(p = runif(1, pLower, pUpper)))
}

#' binomialW
#' 
#' Draw from binomial(num, p)
#' @export
binomialW <- function(n,num,p,...){
	return(rbinom(n,num,p))
}
#' binomialWParm
#' 
#' Generate parameters for \code{binomial}
#' @export
binomialWParm <- function(pLower = 0.1, pUpper = 0.9,
                          numLower = 2, numUpper = 10){
	return(list(p = runif(1, pLower, pUpper),
	            num = round(runif(1, numLower - 0.5, numUpper + 0.5))))
}
#' normalW
#' 
#' Draw from normal(mean,sd)
#' @export
normalW <- function(n,mean,sd,...){
	return(rnorm(n,mean,sd))
}
#' normalWParm
#' 
#' Generate parameters for \code{normal}
#' @export

normalWParm <- function(meanLower = -2, meanUpper = 2,
                       sdLower = 0.25, sdUpper = 2){
	return(list(mean = runif(1, meanLower, meanUpper),
	            sd = runif(1, sdLower, sdUpper)))
}
#' gammaW
#' 
#' Draw from gamma(a,b) distribution
#' @export
gammaW <- function(n,a,b,...){
	return(rgamma(n,a,b))
}
#' gammaWParm
#' Generate parameters for \code{gammaDist}
#' @export

gammaWParm <- function(aLower = 0.5, aUpper = 2.5,
                      bLower = 0.5, bUpper = 2.5){
	return(list(a = runif(1, aLower, aUpper),
	       b = runif(1, bLower, bUpper)))
}

#' normalWCor 
#' 
#' Draw normal variate with mean x (to induce correlation)
#' @export
normalWCor <- function(n,x,sd,...){
	return(rnorm(n,x,1))
}
#' normalWCorParm
#' 
#' Generate parameters for \code{normalCor} (smaller SD = more correlation)
#' @export
normalWCorParm <- function(sdLower = 0.25, sdUpper = 2){
	return(list(sd = runif(1, sdLower, sdUpper)))
}

#' bernoulliWCor
#' 
#' Draw bernoulli variable with prob x/r (to induce correlation)
#' @export
bernoulliWCor <- function(n,x,r,...){
	return(rbinom(n,1,plogis(x/r)))
}

#' bernoulliWCorParm
#' 
#' Generate parameters for \code{bernoulliCorParm} (smaller r = more correlation)
#' @export

bernoulliWCorParm <- function(rLower = 0.5, rUpper = 2){
	return(list(r = runif(1, rLower, rUpper)))
}

#' uniformWCor
#' 
#' Draw variable that is x + uniform error (to induce correlation)
#' @export
uniformWCor <- function(n,x,lower,upper,...){
	return(x + runif(n,lower,upper))
}
#' uniformWParm
#' Generate parameters for \code{uniformParm} (smaller lower = more correlation)
#' @export

uniformWCorParm <- function(lowerLower = -2, lowerUpper = 0,
                        rangeLower = 0.25, rangeUpper = 5){
	lower <- runif(1, lowerLower, lowerUpper)
	upper <- lower + runif(1, rangeLower, rangeUpper)
	return(list(lower = lower, upper = upper))
}